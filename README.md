# adios
Adios is a simple prototype-based object system for Chez Scheme that supports single inheritance.

## Examples

```scheme
(import (scheme)
        (adios))

(define-class vehicle '()
  (lambda (door-count wheel-count capacity)
    (define-field this door-count door-count)
    (define-field this wheel-count wheel-count)
    (define-field this capacity capacity)))

(define-method vehicle (get-door-count)
  (send this door-count))

(define-method vehicle (get-capacity)
  (send this capacity))

(define-method vehicle (get-wheel-count)
  (send this wheel-count))

(define-method vehicle (honk) "Honk!")

(define-class motorcycle vehicle
  (lambda ()
    (super 0 2 1)))

(define-method motorcycle (honk) "Toot!")

(define-class bus vehicle
  (lambda (capacity)
    (super 2 4 capacity)))

(define-method bus (honk)
  (string-append "Honk " (super)))

(define-field bus floor-count 1)

(define-method bus (get-floor-count)
  (send this floor-count))

(define-class double-decker-bus bus
  (lambda ()
    (super 66)
    (define-field this floor-count 2)))
```

---

```scheme
> (define my-bike (new motorcycle))
> (send my-bike get-capacity)
1
> (send my-bike get-door-count)
0
> (send my-bike get-wheel-count)
2
> (send my-bike honk)
"Toot!"
> (define my-bus (new double-decker-bus))
> (send my-bus get-capacity)
66
> (send my-bus get-wheel-count)
4
> (send my-bus get-floor-count)
2
> (send my-bus honk)
"Honk Honk!"
```
