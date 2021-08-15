# Camelfuck: A brainfuck interpreter in Ocaml

They say Ocaml is the right tool for building compilers and interpreters, 
so I went there and tried it for myself. Indeed, a powerful tool fit for the job.

## What is brainfuck?

To quote Urban Müller, its creator:

> The brainfuck compiler knows the following instructions:
>
> Cmd | Effect                               
> --- | ------                               
> \+  | Increases element under pointer      
> \-  | Decrases element under pointer       
> \>  | Increases pointer                    
> <   | Decreases pointer                    
> [   | Starts loop, flag under pointer      
> ]   | Indicates end of loop                
> .   | Outputs ASCII code under pointer     
> ,   | Reads char and stores ASCII under ptr

## Preparing your environment to run this interpreter

- First, follow the instructions on [Ocaml.org](https://ocaml.org/docs/install.html) in order 
to install the Ocaml compiler
- Then, execute the following

```
opam install ocamlbuild
``` 

- Finally, build the project:
```
ocamlbuild brainfuck.ml
```

## How to interpret a brainfuck program

```
./brainfuck.byte /path/to/brainfuck_program
```
