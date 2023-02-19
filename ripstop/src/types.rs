use crate::ast::ASTNode;
use crate::ast::ASTNodeType;
use crate::ast::ASTType;
use crate::error::CompileResult;
use crate::tree::NodeId;
use crate::tree::Tree;
use bimap::BiHashMap;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt::Display;
use std::sync::Arc;

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum Type {
    None,
    Bit,
    Bits { size: usize },
    Literal { minimum_size: usize },
    Struct(Arc<StructDefinition>),
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Type::Bit => "bit".to_string(),
            Type::Bits { size } => format!("bits<{size}>"),
            Type::Literal { minimum_size } => format!("int literal ({minimum_size} bits)"),
            Type::Struct(s) => format!("{}", s.name),
            //Alternatively, `None` could become `_`
            Type::None => "[TYPELESS]".to_string(),
        };
        write!(f, "{}", s)
    }
}

impl Type {
    pub fn bit_size(&self) -> usize {
        match self {
            Self::None => {
                panic!("None type doesn't have a size?");
            }
            Self::Bit => 1,
            Self::Bits { size } => *size,
            Self::Literal { minimum_size } => *minimum_size,
            Self::Struct(s) => s.fields.iter().map(|f| f.member_type.bit_size()).sum(),
        }
    }

    pub fn bitvec(nbits: usize) -> Self {
        Self::Bits { size: nbits }
    }
}

#[derive(Debug, Clone)]
pub struct PrimordialStructField {
    pub name: String,
    pub ast_type: ASTType,
}

#[derive(Debug, Clone)]
/// The primordial struct definition is an intermediary definition that later gets turned into actual struct defs.
/// The reason we need the distinction is because structs may not be declared in a particular order, and
/// structs may reference other structs: Thus, we need to build a DAG of structs in order to completely
/// parse them.
pub struct PrimordialStructDefinition {
    pub name: String,
    pub fields: Vec<PrimordialStructField>,
}

impl PrimordialStructDefinition {
    pub fn from_ast(ast: &Tree<ASTNode>, node: NodeId) -> CompileResult<Self> {
        let mut result = CompileResult::new();
        let head = ast.get_node(node).unwrap();
        let name = match &head.data.node_type {
            ASTNodeType::StructDefinition { name } => name.clone(),
            _ => panic!(),
        };
        let fields: Vec<_> = head
            .children
            .iter()
            .map(|child| {
                let child = ast.get_node(*child).unwrap();
                match &child.data.node_type {
                    ASTNodeType::StructVariable { name, member_type } => PrimordialStructField {
                        name: name.to_string(),
                        ast_type: member_type.to_owned(),
                    },
                    _ => panic!(),
                }
            })
            .collect();
        result.ok(PrimordialStructDefinition {
            name: name,
            fields: fields,
        });
        result
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StructField {
    pub name: String,
    pub member_type: Type,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StructDefinition {
    pub name: String,
    pub fields: Vec<StructField>,
}

impl PartialEq for StructDefinition {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl StructDefinition {
    fn from_primordial(
        primordial: &PrimordialStructDefinition,
        parsed_structs: &[Arc<StructDefinition>],
    ) -> Self {
        let fields: Vec<_> = primordial
            .fields
            .iter()
            .map(|field| {
                let t = if field.ast_type.name == "bit" {
                    Type::Bit
                } else if field.ast_type.name == "bits" {
                    Type::Bits {
                        size: field.ast_type.generic_parameter.unwrap(),
                    }
                } else {
                    let s = parsed_structs
                        .iter()
                        .find(|s| s.name == field.ast_type.name);
                    if s.is_none() {
                        // TODO: Make an error
                        panic!("Couldn't find type {}", field.ast_type.name);
                    }
                    Type::Struct(s.unwrap().clone())
                };
                StructField {
                    name: field.name.clone(),
                    member_type: t,
                }
            })
            .collect();
        Self {
            name: primordial.name.clone(),
            fields: fields,
        }
    }
}

pub struct TypeDatabase {
    //structs: Vec<PrimordialStructDefinition>,
    structs: HashMap<String, Type>,
}

impl TypeDatabase {
    fn build_dag(
        structs: &Vec<PrimordialStructDefinition>,
    ) -> (daggy::Dag<u32, u32>, BiHashMap<daggy::NodeIndex, String>) {
        let mut dag = daggy::Dag::<u32, u32>::new();

        // First insert all the nodes
        let mut node_mapping = BiHashMap::new();
        structs.iter().for_each(|s| {
            if node_mapping.contains_right(&s.name) {
                // TODO: Make an error
                panic!("Module {} is defined twice!", s.name);
            }
            if s.name == "bit" || s.name == "bits" {
                // TODO: Make an error
                panic!("Cannot have a struct named '{}'!", s.name);
            }
            let node_id = dag.add_node(1);
            node_mapping.insert(node_id, s.name.clone());
        });

        // Now insert an edge from each member to its parent
        structs.iter().for_each(|s| {
            s.fields.iter().for_each(|field| {
                let field_type_name = &field.ast_type.name;
                if field_type_name == "bit" || field_type_name == "bits" {
                    return;
                }
                if dag
                    .add_edge(
                        *node_mapping.get_by_right(field_type_name).unwrap(),
                        *node_mapping.get_by_right(&s.name).unwrap(),
                        1,
                    )
                    .is_err()
                {
                    // TODO: Make an error
                    panic!("Cyclical struct definitions");
                }
            });
        });

        (dag, node_mapping)
    }

    pub fn new(structs: Vec<PrimordialStructDefinition>) -> Self {
        let (dag, mapping) = Self::build_dag(&structs);

        // Topo-sorting here should not error, because we found cycles already in build_dag
        let mut parsed_structs = vec![];
        let sorted = daggy::petgraph::algo::toposort(&dag, None).unwrap();
        for nodeid in sorted.iter() {
            // Find the struct def
            let s = structs
                .iter()
                .find(|s| &s.name == mapping.get_by_left(nodeid).unwrap())
                .unwrap();
            parsed_structs.push(Arc::new(StructDefinition::from_primordial(
                s,
                &parsed_structs,
            )));
        }

        TypeDatabase {
            structs: parsed_structs
                .iter()
                .map(|s| (s.name.clone(), Type::Struct(s.clone())))
                .collect(),
        }
    }

    pub fn lookup(&self, ast_type: &ASTType) -> Option<Type> {
        if ast_type.name == "bit" && ast_type.generic_parameter.is_none() {
            Some(Type::Bit)
        } else if ast_type.name == "bits" && ast_type.generic_parameter.is_some() {
            Some(Type::Bits {
                size: ast_type.generic_parameter.unwrap(),
            })
        } else {
            self.structs.get(&ast_type.name).cloned()
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn mk_psf(name: &str, typename: &str, generic: Option<usize>) -> PrimordialStructField {
        PrimordialStructField {
            name: name.to_string(),
            ast_type: ASTType {
                name: typename.to_string(),
                generic_parameter: generic,
            },
        }
    }

    fn lookup(tdb: &TypeDatabase, key: &str) -> Type {
        tdb.lookup(&ASTType {
            name: key.to_string(),
            generic_parameter: None,
        })
        .unwrap()
    }

    fn lookup_generic(tdb: &TypeDatabase, key: &str, generic: Option<usize>) -> Type {
        tdb.lookup(&ASTType {
            name: key.to_string(),
            generic_parameter: generic,
        })
        .unwrap()
    }

    #[test]
    fn default_type_db() {
        let tdb = TypeDatabase::new(vec![]);
        let bits5 = lookup_generic(&tdb, "bits", Some(5));
        assert_eq!(bits5.bit_size(), 5);
    }

    #[test]
    fn struct_type_db() {
        let tdb = TypeDatabase::new(vec![PrimordialStructDefinition {
            name: "foo".to_string(),
            fields: vec![mk_psf("field1", "bits", Some(5))],
        }]);
        let foo = lookup(&tdb, "foo");
        assert_eq!(foo.bit_size(), 5);
    }

    #[test]
    fn type_db_struct_field() {
        let tdb = TypeDatabase::new(vec![
            PrimordialStructDefinition {
                name: "baz".to_string(),
                fields: vec![mk_psf("field5", "bits", Some(27))],
            },
            PrimordialStructDefinition {
                name: "foo".to_string(),
                fields: vec![mk_psf("field1", "bar", None), mk_psf("field2", "bit", None)],
            },
            PrimordialStructDefinition {
                name: "bar".to_string(),
                fields: vec![
                    mk_psf("field3", "bits", Some(17)),
                    mk_psf("field4", "baz", None),
                ],
            },
        ]);
        let foo = lookup(&tdb, "foo");
        assert_eq!(foo.bit_size(), 1 + 17 + 27);
    }
}
