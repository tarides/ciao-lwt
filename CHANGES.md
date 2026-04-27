# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## 0.2

- Compatibility with OCaml 5.4 and 5.5

- Generate `Promise.await` for bound expression that are not function
  applications.
  For example,
  ```ocaml
  let x = async_operation () in
  let* y = x in
  ..
  ```
  is transformed to:
  ```ocaml
  let x = async_operation () in
  let y = Promise.await x in
  ..
  ```
  This will likely result in a compiler error, that can easily be fixed instead
  of a silent change in the program behavior.

## 0.1

Initial release
