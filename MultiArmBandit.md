<h5>Multi-Armed Bandit Problem Implementation Details</h5>

Multi-Armed bandit problem is a well-studied problem, with many algorithms available for solving this problem. The objective of this implementation is to create a flexible environment that is open to algorithm expansion in scala.
  
  
  Assuming 10 arm bandit - this configuration can be changed easily

  arm: contains features of the bandit

  Algorithm: four althorithms are implemented by typeclass Algorithm: epsilon greedy, incremental, gradient, and ucb

  scalanlp's breeze is used for numerical computation, and breeze-viz for data visualization

  ExtendedRand is an extension of breeze Rand, which adds "weighted choose by index" from an Iterable, or typically, DenseVector.


Epsilon Greedy:

![Alt text](Reinforcement.Learning/epsilon_greedy_average.png?raw=true "Epsilon Greedy average")
![Alt text](Reinforcement.Learning/epsilon_greedy_best_action.png?raw=true "Epsilon Greedy best action")

Gradient:

![Alt text](Reinforcement.Learning/gradient_average.png?raw=true "Gradient average")
![Alt text](Reinforcement.Learning/gradient_best_action.png?raw=true "Gradient best action")

Incremental

![Alt text](Reinforcement.Learning/incremental_average.png?raw=true "Incremental average")
![Alt text](Reinforcement.Learning/incremental_best_action.png?raw=true "Incremental best action")

Upper-Confidence-Bound

![Alt text](Reinforcement.Learning/ucb_average.png?raw=true "UCB average")
![Alt text](Reinforcement.Learning/ucb_best_action.png?raw=true "UCB best action")

Baysean
![Alt text](Reinforcement.Learning/baysean-regrets.png?raw=true "Baysean Regrets")
