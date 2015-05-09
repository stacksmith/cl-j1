J1 is an stack machine FPGA cpu created by James Bowman (see //http://www.excamera.com/sphinx/fpga-j1.html).  cl-j1 is an assembler and emulator for the processor written in Common Lisp.

Summary:

J1 CPU is a 16-bit stack machine.  The instructions are 16 bits in length, and other than jumps and literal loads, look like a VLIW instructions.  The bits control transfers and stack pointer modifications.  Since there are few registers in a stack machine, 12 bits control the entire machine.  The instruction set is very flexible, and many instructions map closely onto Forth primitives.

cl-j1 contains an assembler that writes code directly into a memory image, and an emulator that executes code in the memory image.

Status:

Early working version.  Simple counting loop assembles and executes (around 1 million instructions per second - not bad as no effort has been made to optimise).
