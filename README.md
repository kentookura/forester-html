# Quickstart

Put some trees in `./static` and run `dune exec --watch forester_html`

# What

An alternative rendering architecture for forester which uses the excellent
[Dream]() and [Dream-html]() libraries and [HTMX]() instead XML and XSLT.

Coincidentally, this repo kind of functions as an ocaml implementation
of a development server for forester:
running `dune exec --watch forester_html` shows the compilation errors and
hosts the forest (although currently not everything is being rendered).
The downside is obviously that the functionality is not "installable",
meaning you need to run `dune` in the repo for this to work.

# Why

HTMX extends HTML by allowing any element to perform HTTP transactions and
allowing the programmer to replace any element in the DOM instead of the entire
page. This means we can just write a bunch of functions `f: forest -> HTML` and
use them in the UI in a flexible manner, allowing for more interactivity. 

Furthermore, it allows us to get and render trees at runtime, which is useful
if I want to transclude trees from other forests.

# Possibilities

- Managing forests from the frontend via http calls
 
- cross-forest transclusions should (!?) be easy?

- Teacher-student interactions

- Showing diagnostics in the browser at relevant locations.

# Blockers

I haven't figured out if it is possible to keep the server running
when a fatal error occurs. If we want to report errors to the browser
we need to keep running on what `forester` says are fatal errors


# References
[Hypermedia Systems by Carson Gross, Adam Stepinski and Deniz Akşimşek](https://hypermedia.systems/)
