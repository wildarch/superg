# Benchmarking Miranda
## Building from source
Building Miranda using Bazel is non-trivial, so instead, we provide manual instructions for building it from source.

Main download site: https://www.cs.kent.ac.uk/people/staff/dat/miranda/downloads/
Download link is HTTP and therefore does not work in Chrome.

The resolved URL is https://www.cs.kent.ac.uk/people/staff/dat/miranda/src/mira-2066-src.tgz.

Extract the file and enter the directory `miranda`.
Miranda requires `byacc` to build, install it first with `sudo apt install byacc`.
When you run `make`, we get errors about multiple definitions to various lexer symbols.
To work around that, we patch `Makefile` (generated with `diff -Naur`):

```patch
--- Makefile	2020-01-31 15:52:50.000000000 +0100
+++ Makefile	2022-11-27 17:48:40.594670539 +0100
@@ -12,6 +12,9 @@
 CFLAGS = #-O #-DCYGWIN #-DUWIN #-DIBMRISC #-Dsparc7 #-Dsparc8
 #be wary of using anything higher than -O as the garbage collector may fall over
 #if using gcc rather than clang try without -O first
+# Paper over an issue where variables in a header file are included multiple times 
+# and thus defined more than once.
+LDFLAGS=-Wl,--allow-multiple-definition
 EX = #.exe        #needed for CYGWIN, UWIN
 YACC = byacc #Berkeley yacc, gnu yacc not compatible
 # -Dsparc7 needed for Solaris 2.7
@@ -20,7 +23,7 @@
 			    version.c miralib/.version fdate .host Makefile
 	$(CC) $(CFLAGS) -DVERS=`cat miralib/.version` -DVDATE="\"`./revdate`\"" \
 	    -DHOST="`./quotehostinfo`" version.c cmbnms.o y.tab.o data.o lex.o \
-	    big.o reduce.o steer.o trans.o types.o utf8.o -lm -o mira
+	    big.o reduce.o steer.o trans.o types.o utf8.o -lm -o mira $(LDFLAGS)
 	strip mira$(EX)
 y.tab.c y.tab.h: rules.y
 	$(YACC) -d rules.y
```

Run the patch from within `miranda`:

```shell
patch -p0  < experiments/superg/mirabench/multiple-definition.patch
```

Now `make` should run successfully:

```shell
make
gcc -w    -c -o big.o big.c
gcc -w    -c -o cmbnms.o cmbnms.c
gcc -w    -c -o data.o data.c
gcc -w    -c -o lex.o lex.c
gcc -w    -c -o reduce.o reduce.c
gcc -w    -c -o steer.o steer.c
gcc -w    -c -o trans.o trans.c
gcc -w    -c -o types.o types.c
gcc -w    -c -o utf8.o utf8.c
gcc -w    -c -o y.tab.o y.tab.c
gcc -w  -DVERS=`cat miralib/.version` -DVDATE="\"`./revdate`\"" \
    -DHOST="`./quotehostinfo`" version.c cmbnms.o y.tab.o data.o lex.o \
    big.o reduce.o steer.o trans.o types.o utf8.o -lm -o mira -Wl,--allow-multiple-definition
strip mira
gcc -w  menudriver.c -o miralib/menudriver
chmod 755 miralib/menudriver
strip miralib/menudriver
compiling /home/daan/Downloads/miranda/miralib/stdenv.m
checking types in /home/daan/Downloads/miranda/miralib/stdenv.m
compiling ex/ack.m
checking types in ex/ack.m
...
```

## Collecting statistics
Miranda does not have a built-in benchmarking mechanism, so we add a small patch that prints the reduction time and the number of reduction steps.

```
diff --git a/reduce.c b/reduce.c
index 24c3241..8f2ab5c 100644
--- a/reduce.c
+++ b/reduce.c
@@ -12,6 +12,7 @@
 #include <errno.h>
 #include <sys/types.h>
 #include <sys/stat.h>
+#include <time.h>
 struct stat buf;  /* used only by code for FILEMODE, FILESTAT in reduce */
 #include "data.h"
 #include "big.h"
@@ -224,7 +225,22 @@ L:e= reduce(e);
 /* ### */
 void print(e) /* evaluate list of chars and send to s_out */
 word e;
-{ e= reduce(e);
+{ 
+  struct timespec start;
+  long long start_cycles = cycles;
+  clock_gettime(CLOCK_MONOTONIC, &start);
+  e= reduce(e);
+  struct timespec end;
+  long long end_cycles = cycles;
+  clock_gettime(CLOCK_MONOTONIC, &end);
+  fprintf(stderr, "Start: %lld.%.9ld\n", (long long)start.tv_sec, start.tv_nsec);
+  fprintf(stderr, "End: %lld.%.9ld\n", (long long)end.tv_sec, end.tv_nsec);
+
+  long long seconds = ((long long) end.tv_sec) - ((long long)start.tv_sec);
+  long long nanos = end.tv_nsec - start.tv_nsec + (seconds * 1000000000ll);
+  fprintf(stderr, "Nanos: %lld\n", nanos);
+  fprintf(stderr, "Cycles: %lld\n", end_cycles - start_cycles);
+
   while(tag[e]==CONS && is_char(hd[e]=reduce(hd[e])))
   { unsigned c=get_char(hd[e]);
     if(UTF8)outUTF8(c,s_out); else

```

We use the high-resolution monotonic clock. [Rust uses the same source](https://doc.rust-lang.org/std/time/struct.Instant.html#underlying-system-calls).
For consistency, we invoke `miranda` from Rust as a subprocess, and use `criterion` to analyze the results.
Apply this patch with:

```shell
patch -p1  < experiments/superg/mirabench/reduce-bench.patch
```

Then run `make` again.
To check that it works, run `miranda` with:

```shell
$./mira ex/fib.m 


                                 T h e   M i r a n d a   S y s t e m

                              version 2.066 last revised 17 January 2023

                              Copyright Research Software Ltd 1985-2020

                                World Wide Web: http://miranda.org.uk


ex/fib.m
for help type /h
Miranda fib 30
Start: 12176.001154171
End: 12176.514162938
Nanos: 513008767
Cycles: 18304863
832040
```
