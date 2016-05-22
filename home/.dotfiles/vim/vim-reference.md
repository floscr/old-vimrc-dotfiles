Vim Reference Sheet
===================


## Tabularize

Indent a long variable declaration

```js
var longString = 'Hello, ' +
                 'this ' +
                 'is ' +
                 'a long' +
                 'string';
```

`:Tabularize /^\(.\+=\)\?\s/l0`
