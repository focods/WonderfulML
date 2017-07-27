import numpy as np
import matplotlib.pyplot as plt

def normald(X=None, mu=None, sigma=None):
    """ Returns the normal probability density for a D-dimensional vector X
        X - n (samples) x d (dimensions) matrix (2D numpy array) of samples
        mu - 1 x d matrix of the d-dimensional mean of the distribution
        sigma - d x d covariance matrix of the d-dimensional distribution
        
        If no samples (xs) are supplies, just a definition is returned.
        If no means (mu) are supplied, assume std. norm. dist. w/ all mu=0.
        If no sigma covariance matrix is supplied, assume std. normal dist.
         w/ sigma = the d x d identity matrix
    """
    
    if X == None:
        def_string = " mydnorm(xs, mu, sigma) " \
                     " X: n (samples) x d (dimensions) " \
                     " mu: 1 x d mean vector " \
                     " sigma: d x d covariance matrix "
        return def_string
    
    return 0