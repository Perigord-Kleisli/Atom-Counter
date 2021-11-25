# Atom-Counter
simple program for parsing formula names
![Testcases](https://i.imgur.com/UL2KW17.png)

**No longer being developed, element groups in table mode also doesnt work**


![Table Mode](https://i.imgur.com/MliTM6t.png)
The basic syntax in table mode is
  - Any element on its own does not get affected
  ```
 Au_2 -> Au_2
|----+----------+---------|
|    | Reactant | Product |
|----+----------+---------|
| Au |    2     |    2    |
|----+----------+---------|
```
  - Any element that starts with an '\_' of any length takes a coeffecient
   ```
 Au_2 -> 2 _Au_2
|----+----------+---------|
|    | Reactant | Product |
|----+----------+---------|
| Au |    2     |    4    |
|----+----------+---------|
```
  - A Coefficient is any number not connected to an element, it doesnt need to be before the element with a blank line
```
A_3 -> _A_4 4
|---+----------+---------|
|   | Reactant | Product |
|---+----------+---------|
| A |    3     |   16    |
|---+----------+---------|
```
  - Any character other than letters numbers underscores and arrows gets ignored
```
A_2 | C_2 -> C_2 + _A _A + _A $ 2 2 4
|---+----------+---------|
|   | Reactant | Product |
|---+----------+---------|
| A |    2     |    8    |
|---+----------+---------|
| C |    2     |    2    |
|---+----------+---------|
```
- :q to quit
