# Hypercrud Browser

> navigate app-values like a web browser navigates HTML

Hypercrud is a Clojure and ClojureScript system for building database apps.

### Motivation

Most CRUD apps are 90% the same boilerplate. The things we need backend code for - security, performance, async, and failure handling - are all accidental complexity, unrelated to the application's actual purpose. This boilerplate manifests itself in the [backend-for-frontend pattern](http://samnewman.io/patterns/architectural/bff/) (anti-pattern), whereby each backend is hand-optimized to the performance constraints of the frontend it services, for example hand-optimized database queries to avoid database roundtrips, caching vs consistency tradeoffs.

![](https://i.imgur.com/mq6KaTv.png)

**If we had a general purpuse data server, we would not see this O(n) growth in boilerplate, but alas!** The failure to generalize is a manifestation of the object/relational impedance mismatch and is inherent to the relational model.

### Key insight

Our thesis is that by leveraging full-stack immutability from database to frontend, we can build a fully-general solution that is correct, consistent and yet also maximally performant.

The key is Datomic, the immutable database. Datomic does not have an object/relational impedance mismatch, and thus can be used as the keystone component of a fully general data server. A single backend server which can service many frontends, with different data needs, without sacrificing consistency or performance. If you'd like to understand this claim, you might start with the [Datomic documentation](http://docs.datomic.com/getting-started/brief-overview.html). Henceforth in this document, I take this claim for granted, and will defend it in a separate blog post which isn't yet published.

### Implications

A general backend data server solves performance, async, and failure handling. Performance concerns no longer dictate that we run our application code on the server. Security is the only remaining application code that needs to run trusted. Datomic includes a way to run a small security kernel inside the database. Read security is enforced inside the query process, and write security inside the transactor process.

All our business rules, our queries and forms and transactions, all things typically done in a backend, now can be pushed into the client (often a web browser).

This is an enormous architectural change, with profound and astounding implications as to the way we architect our applications.

## Hypercrud Client, Hypercrud Server

Hypercrud Client is an I/O runtime for efficient client-server data sync with Hypercrud Server. Inspired by Om Next, the userland interface is two functions.

* a request function to specify data dependencies
* a view function (React.js expression)

The runtime will fetch the data dependencies as specified by the request function, and then pass that value to the view. By sequestering I/O to the fringe of the system, we are left with a composable programming model of pure functions against local data. **Userland code experiences no async, no failures, no latency.**

#### What does userland code look like?

```clojure
(def request-blog
  (->QueryRequest '[:find ?post :where [?post :post/title]]
                  {"$" #DbVal[:samples-blog]}
                  {"?post" [#DbVal[:samples-blog] ['*]]}))

(defn request [state peer]
  [request-blog])

(defn view [state peer dispatch!]
  (-> (let [result @(hc/hydrate peer request-blog)]     ; synchronous
        ; result looks like
        ; [{"?post" {:db/id #DbId[17592186045419 17592186045786], :post/title "First blog post"}}
        ;  {"?post" {:db/id #DbId[17592186045420 17592186045786], :post/title "Second blog post"}} ... ]
        [:ul
         (->> result
              (map (fn [relation]
                     (let [post (get relation "?post")]
                       [:li {:key (:db/id post)}
                        (:post/title post)]))))])))
```

The runtime will manage the lifecycle of these functions and render your application:

![](http://i.imgur.com/zwoGq2I.png)

All decision making is driven by the client. The client programming experience is that of composing functions and values, which makes it a straightforward exercise to model the app as an EDN value to be interpreted by the client.

### Hypercrud Browser

Hypercrud Browser navigates app-values like a web browser navigates HTML. **The things we generally have to write code for - security, performance, async, and failure handling - are all accidental complexity.** When you take that away, we're left with the very simple essence of an application's true business domain. For this, a simple DSL will do, the simpler the better. We're left with the essense of an application, as a value. App-values define Pages, each Page declares his data dependencies, and Links to other Pages.

Interpreted app-value in a web browser, with custom renderers disabled:

![](http://i.imgur.com/f1ngGLt.png)

Same app-value but as the end user sees it, respecting renderers:

![](http://i.imgur.com/4WlmuW8.png)

Here's the raw app-value:

```edn
{:page/name "Sample Blog",
 :page/query "[:find ?post :in $ :where [?post :post/title]]",
 :page/dbs [{:dbhole/name "$", :dbhole/value {:database/ident "samples-blog",}}],
 :page/find-elements [{:find-element/name "?post",
                       :find-element/connection {:database/ident "samples-blog"}
                       :find-element/form {:form/name "samples-blog - post",
                                           :form/field [{:field/prompt "title",
                                                         :field/attribute {:attribute/ident :post/title,
                                                                           :attribute/valueType #:db{:ident :db.type/string},
                                                                           :attribute/cardinality #:db{:ident :db.cardinality/one}}}
                                                        {:field/prompt "date",
                                                         :field/attribute {:attribute/ident :post/date,
                                                                           :attribute/valueType #:db{:ident :db.type/instant},
                                                                           :attribute/cardinality #:db{:ident :db.cardinality/one}}}
                                                        {:field/prompt "content",
                                                         :field/attribute {:attribute/ident :post/content,
                                                                           :attribute/valueType #:db{:ident :db.type/string},
                                                                           :attribute/cardinality #:db{:ident :db.cardinality/one}}}]}}]

 :page/links [{:link/prompt "view",
               :link/link {:db/id #DbId[17592186045791 17592186045422], :page/name "view post"},
               :link/repeating? true,
               :link/find-element {:find-element/name "?post", :find-element/connection #:db{:id #DbId[17592186045786 17592186045422]}},
               :link/ident :sys-edit-?post}
              {:link/prompt "new",
               :link/ident :sys-new-?post,
               :link/repeating? false,
               :link/find-element {:find-element/name "?post", :find-element/connection #:db{:id #DbId[17592186045786 17592186045422]}},
               :link/render-inline? true,
               :link/page {:db/id #DbId[17592186045791 17592186045422], :page/name "view post"}}]
 :page/renderer "(fn [relations colspec anchors param-ctx] ... )"}
```

You might imagine the code to interpret an app-value to produce a view and a request. This code is called Hypercrud Browser and provided as a library:

```clojure
(def app-value { ... })

(defn view [state peer dispatch!]
  [browser/safe-ui app-value (:route state) {:display-mode :xray :dispatch! dispatch!}])

(defn request [state peer]
  ; code your own data dependencies, or let the browser figure it out from an app-value
  (browser/request app-value (:route state) {:display-mode :xray}))
```

Pages compose by composing their data dependencies therein (like an iframe), and are thus a scalable model for building UIs. Hypercrud Browser is HATEOAS.

![](http://i.imgur.com/4mKpHhw.png)

App-values are graph-shaped and grow to be quite large. It is natural to want to store app-values in a database, and create tooling to build these up visually and interactively, which leads us to:

### Hyperfiddle.net

[hyperfiddle.net](http://hyperfiddle.net/) is a WYSIWYG editor for building Hyperfiddle app-values ("hyperfiddles"). It is also a better Datomic Console - an interactive query builder, entity explorer and can be attached to an arbitrary Datomic database without alterations. It heavily leans on d/with as a transaction staging area, including a notion of branching and discard.

![](http://i.imgur.com/v3cmewv.png)

### FAQ

**What about database schema?** Datomic schema is also a value, you can interactively build schema values and apply them without restarts. It really helps to have branching and discard here, so you can experiment with your schema in the browser before transacting the change.

Datomic Peer or Datomic Client?