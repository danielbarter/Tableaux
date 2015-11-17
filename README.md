# Tableau
Tableaux is a game which demonstrates some of the combinatorics which appears when you study the irreducible representations of the symmetric group.

To play the game, you need a haskell compiler. 

Run `main :: IO ()`. You will be asked for a weakly decreasing sequence. Here are some examples of valid inputs:

```
[7]
[1,1,1]
[3,3,2,1]  
[3,2,1,1]  
[4,4,4,3,1]  
```

Once you have chosen your sequence, the game starts. If you chose [3,3,2,1], you will see the following pattern:

```
1 2 3  
4 5 6  
7 8  
9  
```

the valid transformations in this case are 1,2,3,4,5,6,7,8.

At any stage in the game, some of these transformations may have no effect. You win once you reach the configuration

```
1 5 8  
2 6 9  
3 7  
4  
```
