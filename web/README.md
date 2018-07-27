# gwn-scala-api-web #

## Build & Run 

```sh
$ cd gwn-scala-api-web
$ sbt
> jetty:start
> browse
```

If `browse` doesn't launch your browser, manually open [http://localhost:8080/](http://localhost:8080/) in your browser.

## Compile

```
cd ..
sbt publishLocal
cd -
```

Modify the base path at `src/main/twril/layouts/default.scala.html`

```
sbt package
```
