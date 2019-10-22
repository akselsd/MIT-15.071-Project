# MIT-15.071-Project

### General Git workflow

1. Use ```git pull``` to pull (download) the latest commits (changes) to your local repository
2. Make changes in the code.
3. When you feel like you have reached a natural stopping point (fixed a bug, added a plot/new feature etc) use ```git add``` to label those changes as ready to be commited. Use either ```git add filename``` to add a spesific file e.g. ```git add plots.R``` or just use ```git add .``` to add all changed files. 
4. Create the commit using ```git commit -m "Description"``` e.g. ```git commit -m "Added histogram of weather delay"```. This will take all the changes you previously added and bundle them together in a commit. 
5. Use ```git push``` to push (upload) the commit to the remote repository on github.

Useful general commands
- ```git status``` to see the status of your local repository
- ```git log``` to see the commit history
