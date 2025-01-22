# Hide zh in e-pub

This utility will remove duplicates paragraph in zh and leave only jp the original paragraphs.
The zh paragraphs serve as translation line by line to the original jp but not
for folks who only look to original jp paragraphs.  

Since this is part of exampleriments, there's no release page for this hence when
users want to use this, they have to build themselves e.g.

1. Download manually the file `go.mod` and `main.go` to any folder.
2. Move to the folder where those two above placed.
3. Assuming you have the Golang compiler, do this `go mod tidy && go build -o hce.exe .` Note, `hce.exe` is the executable regardless whether you're using Windows, Linux or MacOS.
4. The executable `hce.exe` will ready to use.

With above example, to try it:

```bash
./hce.exe ../where/is/my/file.epub
```

After it's done cleaning up, you will file the `../where/is/my/file_cleaned.epub` (beside the target file)
appended with `_cleaned` in its file name.

# LICENSE
MIT LICENSE