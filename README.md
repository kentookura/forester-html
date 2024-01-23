# Quickstart

Put some trees in `./static` and run `dune exec --watch forester_html`

# What

An alternative rendering architecture for forester which uses the excellent
[Dream]() and [Dream-html]() libraries and [HTMX]() instead XML and XSLT.

# Why

HTMX extends HTML by allowing any element to perform HTTP transactions and
allowing the programmer to replace any element in the DOM instead of the entire
page. This means we can just write a bunch of functions `f: forest -> HTML` and
use them in the UI in a flexible manner, allowing for more interactivity. 

Managing forests from the frontend via http calls

cross-forest transclusions should (!?) be easy?

Teacher-student interactions

[Hypermedia Systems by Carson Gross, Adam Stepinski and Deniz Akşimşek](https://hypermedia.systems/)
