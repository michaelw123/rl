# Reinforcement Learning
This is a research project that explores Reinforcement Learning by solving some of the RL problems with various approaches using Scala, with focus on policy/value systems. We intend to generalize the RL problems into a framework. We choose Scala for its implementation, as we believe that the hybrid of FP and imperative paradigm is best suited to address the complexity of environment/agent in RL.

Given the prolific python implementations to various DL and RL frameworks and algorithms, our research objective is to increase the presence of scala language, and provide an alternative framework and library to this research field.

We plan to start from implementing concepts in Sutton & Barto's book Reinforcement Learning: An Introduction (2nd Edition), and expend our research into more specific areas.


Some of the planned topics are:
* Multi-Armed Bandit problem
* Dynamic Programming 
* Markov Decision Processes
* Monte Carlo Methods
* Temporal-Difference Learning


https://github.com/michaelw123/rl

* Multi-Armed Bandit Problem Implementation Details


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


