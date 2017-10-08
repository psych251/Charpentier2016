# Project Proposal

## Motivation

My research interests include using neuroimaging technique to understand how information is encoded and processed in the brain such as decision-making or affect. My “defined” research interest is somewhat broad currently because I am relatively new to research (sophomore undergrad), but I believe this study is particularly suited for me because it involves designing a task similar to neuroimaging paradigms and analyzing decision-making data. In the end, I think replicating this study will help me learn useful skills that I will definitely use for fMRI studies.

## Description

Participants engage in two types of task, a “feeling task” and a “gambling task”. In the feeling task, participants made 40-48 trials that required them to choose between shapes that were associated with a certain loss and certain gain (between £0.2 and £12), for 4 blocks. Participants were told that each shape had an associated win and loss. In half of the blocks, participants reported how positively or how negatively they would feel if they had loss or won. In the other half, participants reported what they actually felt after their decision. The gambling task comprises of 288-322 trials which required participants to choose between a 50-50 gamble and a sure option. Wins and losses were also between £0.2 and £12. For the analysis of the feeling task, the researcher fitted 10 different models on the feeling task that predicted experienced (or expected) feeling as a function of gains/loss. BIC was used for model selection. Choice model of the gambling task was constructed with the best feeling task model, F(x). Choice model is a fitted logistic regression taking in F(L), F(G), and F(S), where F(L) is the expected affect of the potential loss amount, F(G), where F(G) is the expected affect of the potential gain, and F(S), where F(S) is the expected affect of the sure option.	

## Challenges

One major challenge is that the total time it takes for participants to complete both task is probably around an hour. This is very long and somewhat expensive for an mTURK experiment. I am considering cutting the trials in half. This theoretically should be fine, since their DV is calculated AIC from each model they try. Each model is fitted to one participant, which will be used to calculate an AIC (an observation). Another pr
