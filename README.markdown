# Sinclair - a bespoke blog engine

Sinclair is a dynamic blogging engine. It works by initialising the
engine from disk, loading relevant information into an in-memory hash
map. When it is alerted to update or add a new post/page, it rescans
the appropriate files.

It's a combination of the blogging engine I want and some of the ideas
I want to play with. It is highly unlikely this will be useful to
other people.

## Usage

This requires sinclair to be in Quicklisp's local-projects.

```
(ql:quickload :sexml)
(sexml:with-compiletime-active-layers
    (sexml:standard-sexml sexml:xml-doctype)
  (sexml:support-dtd
   (merge-pathnames "html5.dtd" (asdf:system-source-directory "sexml"))
   :<))
(<:augment-with-doctype "html" "")
(ql:quickload :sinclair)
```
## Author

* K. Isom (kyle@tyrfingr.is)

## Copyright

Copyright (c) 2014 K. Isom (kyle@tyrfingr.is)

# License

Licensed under the ISC License.
