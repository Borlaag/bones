<img src="https://user-images.githubusercontent.com/114830266/210143153-f4a6dae6-6688-445d-ae48-2bd104672496.png" height="500px">
<img src="https://user-images.githubusercontent.com/114830266/210143062-343c48e0-3d43-4d2b-89f8-0919054b675d.png" height="100px">

# A simple Scheme compiler for x86_64 systems

This repository is a mirror of the original source, which is available [here](http://www.call-with-current-continuation.org/bones/).

[HTML rendered documentation](http://www.call-with-current-continuation.org/bones/MANUAL.html) is also available.

## License/Terms of Use

The original author of Bones (Felix Winkelmann) has released the code into the public domain,
but includes some code copied from other sources. Please consult the 
[terms of use](http://www.call-with-current-continuation.org/bones/MANUAL.html#sec-17) section in the documentation.

<details>
<summary>Click to view a copy of the Terms of Use</summary>
Some parts of BONES are not by the author, but have been taken from other sources:

The pretty-printer (pp.scm) is

Copyright (c) 1991, Marc Feeley, Author: Marc Feeley, Distribution restrictions: none

The pattern-matching package was written by Alex Shinn and is in the public domain.

The syntax-expander (alexpand.scm) is

Copyright 2002-2004 Al Petrofsky

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

Neither the name of the author nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

The topological sorting routine in tsort.scm was written by Moritz Heidkamp and is in the public domain.

Everything else was written by Felix Winkelmann and is put into the public domain.
</details>
