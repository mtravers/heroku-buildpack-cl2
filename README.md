Heroku Buildpack for Common Lisp
================================

[ A note to the perplexed -- this is my (mtravers) version of avodonosov's version of my buildpack. It is not a github fork of his because github won't let me do that, and it doesn't replace my original because I want to leave it around to preserve compatibility. 


My rework of the Common Lisp buildpack for Heroku [by Mike Travers](https://github.com/mtravers/heroku-buildpack-cl).

Differences from the Mike's buildpack:
* Doesn't install patched portableaserve and wuwei. You are free to use any webserver.
  If you need a version not available in quicklisp, you can fetch it as a git submodule
  of your application.
* Doesn't force your application to be saved into a lisp image at Heroku.
  We recommend the build output to consist of full application and libraries sources
  together with prebuild .fasl files to speedup loading. You can build a lisp image,
  but often this will lead to problems. For example, many libraries keep static files
  (e.g css, javascript) in their repositories and access the files using
 `asdf:system-relative-pathname`. If lisp image is saved and copied to another
  location (as it happens with build results at Heroku), those libraries will
  not find their static files.

## Usage
To feel comfortable, read about Heroku [Procfile](https://devcenter.heroku.com/articles/procfile)
and [Buildpack](https://devcenter.heroku.com/articles/buildpack-api). You will then understand,
that Heroku allows you to use any unix command to start your application. And buildpack
provides a separate `compile` step, where you can prepare things for that command: fetch
the lisp implementation binary, download libraries with quicklisp and build .fasl files. Again, 
using full power of unix.

The `compile` script of this buildpack installs SBCL available as _sbcl/sbcl-1.0.54-x86-64-linux/run-sbcl.sh_,
and then invokes a _heroku-compile.lisp_ script in your application root directory.

In your _heroku-compile.lisp_ you typically want to prebuild .fasl files of your
application by first `(asdf:disable-output-translations)` and then loading your ASDF system.

If you need Quicklisp, call `cl-user::require-quicklisp`, it installs quicklisp in the 
directory _quicklisp/_.

Your application is started according to what you specfy in your Procfile.
Here you can invoke the SBCL and run the lisp code you need. 
In the Procfile command you also want to disable ASDF output translations.

Disabling ASDF output tranlastions is necessary becuse Heroku performs the `compile`
step on one machine/directory, and then copies the result into another machine/directory.
With default output translations ASDF caches .fasl files according to the full path
of their source files. When the sources are moved to another location, ASDF can not match them
to the cached .fasls. In result full recompilation will hapen at start time of your application.

With ASDF output translations disabled the .fasl files are placed near the sources,
and when copied together, ASDF still matches them.

See the [example application](https://github.com/avodonosov/heroku-cl-example2). 

## Notes
* Heroku does not have a persistent file system. Applications should use S3 for storage; [ZS3](http://www.xach.com/lisp/zs3) is a useful CL library for doing that.
* You can login into the server where your application is deployed and inspect it with `heroku run bash` command. See [one off dynos] (https://devcenter.heroku.com/articles/one-off-dynos).

## Todos
* `require-quicklisp` function should accept desirable quicklisp dist version and quicklisp
  client version, because when deploying application to server we want predictable environement.

## Credits
* [Mike Travers](hyperphor.com) for his great example
* [Jose Santos Martins Pereira](https://github.com/jsmpereira/heroku-buildpack-cl) for hosting SBCL binary tarball at S3 and example of how to deploy it to heroku.
* Heroku and their new [Buildpack-capable stack](http://devcenter.heroku.com/articles/buildpacks)
* [QuickLisp](http://www.quicklisp.org/) library manager 
* All other countless lispers and developers of all generations who made this possible.



