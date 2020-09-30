# grzb

A While verifier powered by Z3

## Example

While programs are written in LISP-y syntax:

```
$ cat examples/mult.while                                                  
(begin
  (assert {init m k})
  (res := 0)
  (while {= (* init-m init-k) (+ res (* m k))}
    (not (= k 0))
    (begin
      (res := (+ res m))
      (k := (- k 1))))
  (assert {= res (* init-m init-k)}))
$ grzb examples/mult.while
ok
```

## Compilation

To compile __grzb__, you need Racket and Z3. For example, the following should do the trick on mac:

```
brew cask install racket
brew install z3
```

Then, make sure to set the `Z3_LIB` variable to point to Z3, e.g.,

```
export Z3_LIB=/usr/local/lib
```

As the next step, install the Racket bindings for Z3:

```
raco pkg install z3
```

Now we are ready to compile __grzb__ for the terminal:

```
cd grzb
raco exe -o grzb src/front-terminal/main.rkt
```

We can check if it works:

```
./grzb examples/factorial.while
```
