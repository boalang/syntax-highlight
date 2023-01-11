# syntax-highlight
Collection of files to support syntax highlighting Boa source files in various editors.

## Emacs Support

Emacs support for Boa source files and projects is split into five
modules.  Place the `*.el` files somewhere on your `load-path` and
`require` as necessary.  In particular, features are as follows:

 - `boa-mode.el` provides `cc-mode` based support for the Boa
   language, including syntax highlighting, abbreviations, pre-defined
   yasnippets, autocompletion, and imenu navigation of functions,
   types and outputs.
 - `boa-doc.el` provides `eldoc` support in Boa, including collection
   of locally defined function signatures.
 - `boa-ide.el` provides support for previewing/executing/downloading
   the results of Boa queries which are part of a study template based
   project.
 - `boa-study-config.el` provides a minor mode for use in
   [`json-mode`](https://github.com/joshwnj/json-mode) which can be
   used to edit the `study-config.json` file, including support for
   `find-file-at-point`, building/running targets, and completion of
   target and file names.
 - `boa-sc-data.el` maintains the data model used by both `boa-ide`
   and `boa-study-config` to access and interact with [study
   template](https://github.com/boalang/study-template) based
   projects.

## a2ps

[a2ps](https://www.gnu.org/software/a2ps) is a program which can
highlight code for hard-copy printing.  The file `boa.ssh` provides
support for this, and may be installed in `~/.a2ps/` for use.
