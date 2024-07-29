# Shared View

Base package used in server components, SSR & application rendering. Everything
in here is generic, and can be used in the exact same way, whether it's on web,
or anywhere else. Just import the package, and start to use it. Take a look in
the siblings folder to get your head around that idea.

## Using it in siblings folder

```toml
# In gleam.toml.
# Leverage on compiler path resolution.
[dependencies]
shared_view = { path = "../shared_view" }
```
