## 0.11.0 (???)

- change signature of `restructure-param` to receive key, value and the accumulator. Remove the key from accumulator parameters by default. Remove alpha-tag.
- more docs on creating own metadata DSLs
- use `clojure.walk16` internally

## 0.10.4 (16.4.2014)

- fixed https://github.com/metosin/compojure-api/issues/12
- added http-kit example

## 0.10.3 (15.4.2014)

- renamed `clojure.walk` to `clojure.walk16`
- writing routes to `swagger` atom happens now at runtime, not compile-time. Works with AOT.

## 0.10.2 (14.4.2014)

- All `compojure.api.core` restructuring are now using `restructure-param` multimethod to allow external extensions. ALPHA.

## 0.10.1 (13.4.2014)

- FIXED https://github.com/metosin/compojure-api/issues/9
  - `swaggered` resources are now collected in order

## 0.10.0 (10.4.2014)

- fixed bug with missing `+compojure-api-request+` when having both compojure descruction & compojure-api desruction in place
- added support for `:body-params` (with strict schema):

```clojure
(POST* "/minus" []
  :body-params [x :- Long y :- Long]
  :summary      "x-y with body-parameters"
  (ok {:total (- x y)}))
```

## 0.9.1 (9.4.2014)

- update `ring-swagger` to `0.8.4` to get better basepath-resolution (with reverse-proxys)

## 0.9.0 (6.4.2014)

- support for Schema-aware `:path-parameters` and `query-parameters`:

```clojure
(GET* "/sum" []
  :query-params [x :- Long y :- Long]
  :summary      "sums x & y query-parameters"
  (ok {:total (+ x y)}))

(GET* "/times/:x/:y" []
  :path-params [x :- Long y :- Long]
  :summary      "multiplies x & y path-parameters"
  (ok {:total (* x y)}))
```

## 0.8.7 (30.3.2014)

- `swagger-ui` index-redirect work also under a context when running in an legacy app-server. Thanks to [Juha Syrjälä](https://github.com/jsyrjala) for the PR.

## 0.8.6 (29.3.2014)

- use `instanceof?` to match records instead of `=` with class. Apps can now be uberwarred with `lein ring uberwar`.

## 0.8.5 (25.3.2014)

- update `ring-swagger` to `0.8.3`, generate path-parameters on client side

## 0.8.4 (25.3.2014)

- update `ring-swagger` to `0.8.1`, all json-schema generation now done there.

## 0.8.3 (15.3.2014)

- coerce return values with smart destructuring, thanks to [Arttu Kaipiainen](https://github.com/arttuka).
- update `ring-http-response` to `0.4.0`
- handle json-parse-errors by returning json
- rewrite `compojure.api.core-integration-test` using `peridot.core`

## 0.8.2 (10.3.2014)

- Swagger path resolution works now with Compojure [regular expression matching in URL parameters](https://github.com/weavejester/compojure/wiki/Routes-In-Detail). Thanks to [Arttu Kaipiainen](https://github.com/arttuka).

```clojure
(context "/api" []
  (GET* ["/item/:name" :name #"[a-z/]+"] [name] identity))
```

- Sets really work now with smart destructuring of `GET*` and `POST*`. Addeds tests to verify.

## 0.8.1 (6.3.2104)

- update `ring-swagger` to `0.7.3`
- initial support for smart query parameter destructuring (arrays and nested params don't get swagger-ui love yet - but work otherwise ok)

```clojure
  (GET* "/echo" []
    :return Thingie
    :query  [thingie Thingie]
    (ok thingie)) ;; here be coerced thingie
```

## 0.8.0 (5.3.2014)

- Breaking change: `compojure.api.routes/defroutes` is now `compojure.api.core/defroutes*` to avoid namespace clashes & promote it's different.
- FIXED https://github.com/metosin/compojure-api/issues/4
  - reverted "Compojures args-vector is now optional with `compojure.api.core` web methods"

## 0.7.3 (4.3.2014)

- removed the Compojure Var pimp. Extended meta-data syntax no longer works with vanilla Compojure but requires the extended macros from `compojure.api.core`.
- update to `Ring-Swagger` to `0.7.2`

## 0.7.2 (3.3.2014)

- date-format can be overridden in the `json-response-support`, thanks to Dmitry Balakhonskiy
- Update `Ring-Swagger` to `0.7.1` giving support for nested Maps:

```clojure
  (defmodel Customer {:id String
                      :address {:street String
                                :zip Long
                                :country {:code Long
                                          :name String}}})
```

- schema-aware body destructuring with `compojure.api.core` web methods does now automatic coercion for the body
- Compojures args-vector is now optional with `compojure.api.core` web methods

```clojure
  (POST* "/customer"
    :return   Customer
    :body     [customer Customer]
    (ok customer))) ;; we have a coerced customer here
```

## 0.7.1 (1.3.2014)

- update `ring-swagger` to `0.7.0`
  - support for `schema/maybe` and `schema/both`
  - consume `Date` & `DateTime` both with and without millis: `"2014-02-18T18:25:37.456Z"` & `"2014-02-18T18:25:37Z"`
- name-parameter of `swaggered` is stripped out of spaces.

## 0.7.0 (19.2.2014)

- update `ring-swagger` to `0.6.0`
  - support for [LocalDate](https://github.com/metosin/ring-swagger/blob/master/CHANGELOG.md).
- updated example to cover all the dates.
- `swaggered` doesn't have to contain container-element (`context` etc.) within, endpoints are ok:

```clojure
  (swaggered "ping"
    :description "Ping api"
    (GET* "/ping" [] (ok {:ping "pong"})))
```

- body parameter in `POST*` and `PUT*` now allows model sequences:

```clojure
  (POST* "/pizzas" []
    :body [pizzas [NewPizza] {:description "new pizzas"}]
    (ok (add! pizzas)))
```

## 0.6.0 (18.2.2014)

- update `ring-swagger` to `0.5.0` to get support for [Data & DateTime](https://github.com/metosin/ring-swagger/blob/master/CHANGELOG.md).

## 0.5.0 (17.2.2014)

- `swaggered` can now follow symbols pointing to a `compojure.api.routes/defroutes` route definition to allow better route composition.
- `compojure.api.sweet` now uses `compojure.api.routes/defroutes` instead of `compojure.core/defroutes`

## 0.4.1 (16.2.2014)

- Fixed JSON Array -> Clojure Set coarcing with Strings

## 0.4.0 (13.2.2014)

- Initial public version
