<h1>JSON Parser Using Haskell</h1>
<p>
  This repository contains a simple, minimalistic Haskell JSON parser written from scratch with the help of using Alex for the lexer.
  This repository was made for the functional programming course
</p>
<p><b>Documentation: </b><a href="#">link should go here</a></p>
  <h2>Contributors:</h2>
  <li>
  Camil Daou
  </li>
  <li>Christina Haber</li>
  <li>Georgio Zoughby</li>

  <h2>Getting Started</h2>
  <p>What you will need:</p>
  <li><a href="https://www.haskell.org/cabal/">Cabal</a></li>
  <li><a href="https://www.haskell.org/ghc/">GHC</a></li>
  <li><a href="https://hackage.haskell.org/package/alex">Alex</a></li>
  <br></br>
<p>To build the project:</p>

<p>

  ```
 cabal v2-update && cabal v2-run
```

</p>

<p>After Building use this command format to run:</p>

<p>

  ```
 cabal v2-run json-parser <file.json> <field.path | ALLFIELDS | FIELDNAMES> [-o output.txt]
```
</p>
<li>Adding the -o with the filename will redirect the output to a file</li>
<li>FIELDNAMES will only print the field names without the values</li>
<li>ALLFIELDS will print all the fields with their values</li>
<li>You can add the field you want to print and it will print only this field</li>

<h2>Examples</h2>

<p>
  
  ```
  cabal v2-run json-parser test.json ALLFIELDS
  ```

</p>
<p>
name: "John Doe"<br>
age: 30.0<br>
email: "johndoe@example.com"<br>
address.street: "123 Main St"<br>
address.city: "Anytown"<br>
address.state: "CA"<br>
address.zip: "12345"<br>
phoneNumbers: [{ type: "home", number: "555-555-5555" }, { type: "work", number: "555-555-5556" }]<br>
isActive: True<br>
createdAt: "2023-10-01T12:00:00Z"<br>
</p>

<p>
  
  ```
  cabal v2-run json-parser test.json FIELDNAMES
  ```

</p>
<p>
name<br>
age<br>
email<br>
address.street<br>
address.city<br>
address.state<br>
address.zip<br>
phoneNumbers<br>
isActive<br>
createdAt<br>
</p>

<p>
  
  ```
  cabal v2-run json-parser test.json name
  ```

</p>
<p>
name: "John Doe"<br>

</p>

