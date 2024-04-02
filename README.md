Some of this code has served it's purpose as a proof of concept:

[[https://git.sr.ht/~jonsterling/ocaml-forester/tree/dream]]
[[https://lists.sr.ht/~jonsterling/forester-devel/%3CA7840ACE-39BF-43F7-B3F9-6206939E9CE1@jonmsterling.com%3E]]

I will still be experimenting with this codebase, as there are still a lot of things worth exploring with Dream+HTMX.

# Quickstart

Put some trees in `./static` and run `dune exec --watch forester_html`

# What

An alternative rendering architecture for forester which uses 

- [Dream](https://aantron.github.io/dream/) 
- [Dream-html](https://ocaml.org/p/dream-html/2.0.0/doc/index.html) 
- [HTMX](https://htmx.org/) 

instead XML and XSLT.

Here is the idea: Instead of rendering the entire forest to HTML in bulk and
serving it statically, we use the Dream http framework and compute HTML
fragments on demand. Jon has said that his large personal forest takes 20?
seconds to build.

# Why

HTMX extends HTML by allowing any element to perform HTTP transactions and
allowing the programmer to replace any element in the DOM instead of the entire
page. This means we can just write a bunch of functions `f: forest -> HTML` and
use them in the UI in a flexible manner, allowing for more interactivity. 

We can send diagnostics rendered to HTML to the browser via
[SSE](https://htmx.org/extensions/server-sent-events) and
[Websockets](https://aantron.github.io/dream/#websockets)

Furthermore, it will allow us to get and render trees at runtime, which is useful
if I want to transclude trees from other forests. 

# Possibilities

- Managing forests from the frontend via http calls (e.g. "create new tree"-button)
 
- cross-forest transclusions should (!?) be easy?

- Teacher-student interactions

- Showing diagnostics in the browser at relevant locations.

# Blockers

I haven't figured out if it is possible to keep the server running
when a fatal Asai error occurs. We want to report errors to the browser
and keep running.

# References
[Hypermedia Systems by Carson Gross, Adam Stepinski and Deniz Akşimşek](https://hypermedia.systems/)
