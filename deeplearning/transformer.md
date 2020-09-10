# Transformer

## Attention
 - Q: Query (output token)
 - K: Key (input token)
 - Relevance = Q * K
   - Relevance sroces are interpretable!
 - V: Value (input token)
 - Out = Softmax(relevance) * V

### Pseudo-code
 - https://gist.github.com/leopd/29786dd4a2a8ba801324b77fee7f4348

### Multi-headed Attention
 - 8 times
 - Lets network learn 8 different semantic meanings of attention
   - e.g. One for grammar, one for vocab, one for conjugation, ...

## Positional Encoding
 - Attention is "bag of words"
 - Input layer: add a word embedding and a position embedding
 - Position can be either learned or fixed
 - Fixed allows extrapolating to longer sequences


## Key Advantages
 - Easier to train, more efficient
 - Transfer Learning works!
   - Pre-trained models can be fine-tuned for new tasks
 - Can be trained on unsupervised text
   - All the world's text data is now valid training data

## Why ReLu works better?
 - Hinton: Allows neuron to express a strong opinion
 - Gradient doesn't saturate (on the high end)
 - Less sensitive to random initialization
   - e.g. for `tanh` or sigmoid, we need to be careful about the initial value so that it could be in-between the range of high gradient
 - Runs great on low precision hardware
   - e.g. it just increase for x > 0
 - Stupidly easy to compute

### Downsides
 - "Dead neurons" == always output zero (fixed by leaky ReLu)
 - Gradient discontinuous at origin (fixed by GELU, which BERT uses)

## Some General Deep Learning Wisdom
 - Different activations don't really matter
   - Don't bothering tyring tanh/sigmoid
 - Different optimizers do matter
   - Adam is fast, but tends to overfit
   - SGD is slow, but gives grate results
   - Sometimes RMSProp works best
   - SWA can easily improve quality
   - AdaTune (https://github.com/awslabs/adatune)

## MagetronLM (https://github.com/NVIDIA/Megatron-LM)
 - Massive Transformer Language Model from NVIDIA
 - 8.3B parameters trained on 512 GPU for 9 days ($450k on EC2)
 - **Re-usable**, **fine-tunable**
