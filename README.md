### t-sne-learning

#### What is t-SNE?

t-Distributed Stochastic Neighbor Embedding (t-SNE) is a non-linear dimensionality reduction algorithm used for exploring high-dimensional data. It maps multi-dimensional data to two or more dimensions suitable for human observation.

t-SNE is a developed based on Stochastic Neighbor Embedding [SNE, NYU SNE Paper](https://cs.nyu.edu/~roweis/papers/sne_final.pdf) method, which is a basic way to embed high dimension real world data. 

Short coming of SNE is that sometimes the boarder of each cluster would not be that clear. The reason of the problem can be clearified into the following points:

1. Symmetry

2. The Crowding Problem: 

In this case, t-SNE applied t-distribution into SNE to help to solve the crowding problem.
