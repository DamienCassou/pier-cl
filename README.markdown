How to
======

You first have to download this project

```bash
# if you have commit access:
git clone git@github.com:DamienCassou/pier-cl.git
# if you don't
git clone git://github.com/DamienCassou/pier-cl.git
```

Then you must download the Pharo VM and image

```bash
./download.sh vm image
```

Finally, to compile a pier file

```bash
./compile.sh file.pier
```

This will hopefully generate a `file.pier.pdf` and `file.pier.html`.
