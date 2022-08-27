use clap::{Parser, Subcommand};
use std::io::{Read, Write};
use std::fs::File;
use subprocess::{Popen, PopenConfig, Redirection};

pub struct Module {
    //
}

impl Module {
    pub fn instantiate(&self) -> Instance {
        Instance::new(self)
    }
}

pub struct Instance<'a> {
    module: &'a Module,
    proc: Popen,
}

impl<'a> Instance<'a> {
    fn new(module: &'a Module) -> Self {
        let mut p = subprocess::Popen::create(&["./a.out"], subprocess::PopenConfig {
            stdin: subprocess::Redirection::Pipe,
            stdout: subprocess::Redirection::Pipe,
            ..Default::default()
        }).unwrap();

        // iverilog prints out this exact string when we open the dumpfile
        // If we could make it not do that, that'd be great
        let hdr = "VCD info: dumpfile dump.vcd opened for output.\n";
        let mut v = vec![0; hdr.len()];
        p.stdout.as_ref().unwrap().read_exact(&mut v).unwrap();

        Self { module, proc: p }
    }

    pub fn reset_step(&mut self, input: u32) -> u32 {
        self.send_command(109);
        self.stdin().write_all(&input.to_be_bytes());
        self.stdin().flush();

        // Reset
        self.send_command(106);
        self.send_command(104);
        let o = self.read_outputs();
        self.send_command(108);
        o
    }

    pub fn step(&mut self, input: u32) -> u32 {
        self.send_command(109);
        self.stdin().write_all(&input.to_be_bytes());
        self.stdin().flush();

        // Clear reset
        self.send_command(107);
        self.send_command(104);
        let o = self.read_outputs();
        self.send_command(108);
        o
    }

    pub fn finish(mut self) {
        self.send_command(105);
        self.proc.terminate().unwrap();
    }

    fn stdin(&self) -> &File {
        self.proc.stdin.as_ref().unwrap()
    }

    fn stdout(&self) -> &File {
        self.proc.stdout.as_ref().unwrap()
    }

    fn send_command(&self, cmd: u8) {
        self.stdin().write_all(&[cmd]).unwrap();
        self.stdin().flush().unwrap();
    }

    fn read_outputs(&self) -> u32 {
        let mut v = [0; 4];
        self.stdout().read_exact(&mut v).unwrap();
        u32::from_le_bytes(v)
    }
}
