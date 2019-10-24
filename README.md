# MIT-15.071-Project

### Getting started with Git
1. Download and install Git
2. Set your email and username
    - ```git config --global user.name "Aksel Danielsen"```
    - ```git config --global user.email akselsd@mit.edu```
3. Clone the remote repository to you local machine with ```git clone https://github.com/akselsd/MIT-15.071-Project.git```

### General Git workflow

1. Use ```git pull``` to pull (download) the latest commits (changes) to your local repository
2. Make changes in the code.
3. When you feel like you have reached a natural stopping point (fixed a bug, added a plot/new feature etc) use ```git add``` to label those changes as ready to be commited. Use either ```git add filename``` to add a spesific file e.g. ```git add plots.R``` or just use ```git add .``` to add all changed files. 
4. Create the commit using ```git commit -m "Description"``` e.g. ```git commit -m "Added histogram of weather delay"```. This will take all the changes you previously added using ```add``` and bundle them together in a commit. 
5. Use ```git push``` to push (upload) the commit to the remote repository on github. You will need to enter your password.
    - If someone else pushed commits to the repository since your last pull, you will not be able to push. This is because we are all working on the same Git branch, meaning there is a conflict. We could avoid this by using multiple branches, but this adds complexity to the workflow in other ways, and it would require you to learn a lot more about Git. For the scope of this project a single branch will be adequate.
    - The easiest way to solve this is with a rebase. Use ```git pull --rebase``` to fetch the new commit(s) to your local repository and make Git attempt to apply(rebase) your newly created commit(s) on top of them. If there is no conflicts, you are done.
    - If there is a conflict, Git will tell you. You must then [resolve the merge conflict](https://help.github.com/en/github/using-git/resolving-merge-conflicts-after-a-git-rebase). After you have resolved the conflicts, run ```git rebase --continue``` to tell Git to complete the rebase.

### Useful general commands
- ```git status``` to see the status of your local repository
- ```git log``` to see the commit history

### General tips
- Git has a steep learning curve, so it is likely that you will get stuck/break something. Don't be afraid to Google for help, but if you can't resolve it on your own and don't want to wait for me to help, you could always make a new folder and use ```git clone https://github.com/akselsd/MIT-15.071-Project.git``` again to get a fresh start.
- The only dangerous thing is to use a command with ```--force``` as this will override a lot of the built in safety mechanisms in Git. Only use this flag if you know what you are doing.
