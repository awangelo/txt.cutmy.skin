Temporary text sharing from the terminal.

```sh
$ curl --data-binary @file.conf txt.cutmy.skin
https://txt.cutmy.skin/{id}
```

```sh
$ curl txt.cutmy.skin/{id}
content here...
```

## Examples

### Upload

```sh
curl --data-binary @file.conf txt.cutmy.skin  # send a file
cat file.conf | curl --data-binary @- txt.cutmy.skin  # stdin
dmesg | curl --data-binary @- txt.cutmy.skin  # any command's output
printf 'hi alice' | curl --data-binary @- txt.cutmy.skin  # a single string
```

### Download

```sh
curl -s txt.cutmy.skin/{id}  # stdout
curl -s txt.cutmy.skin/{id} | rg -i -C 3 "^err"  # pipe
curl txt.cutmy.skin/{id} -o saved.cfg  # save to file
```

Visiting a URL in a browser also shows its contents.

## Limits

Texts live for 30 minutes. The size limit is 1 MiB and you are limited to 3 uploads per 10 min per IP.
