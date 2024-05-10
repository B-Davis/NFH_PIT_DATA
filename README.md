# NFH_PIT_DATA
Shiny web application for up-to-date fish movement data

## Check-in/out Board

**Brian** - 05/09/2024 going to change file structure and work on PTAGIS grab page....possibly add a button that refreshes data
**Brook** - 05/10/2024-05/10/2024 added additional PITAGIS data to PTAGIS grab page. I wasn't sure if you were done yet, I hope I didn't mess anything up. I also have a bunch of data massaging taking place in here. Maybe we can talk about where to put this and how to structure the global.R file.

How to use this tool:
Warm Springs NFH annual detections at Bonneville Dam
Previous annual detections
cumulative detections
10-year span
juvenile travel time
juvenile survival to Bonneville
SAS for each brood year
annual release and adult return

# Brian's discussion notes for Tue 05/06/24 meting

### Update Button

Reconsidering nightly data downloads, because if housed on Ecos server would require setting up a task manager job which is likely too big of ask (**there is package in r, taskscheduleR that might work, but possibly not**), or setting up a computer in bat-cave to download nightly and push to github or something, then pulling nightly from Ecos (this may also lead to complications). Button would be more streamlined logistically, but would require speed optimization. Ideas: Button will say when last download occurred and give an "are you sure?" message to prevent unneccesary data grabs. Possibly having different data update buttons for different tasks to reduce download times.

### Glbal.R

All data downloads should be occurring outside of global.R file to prevent uneeded downloads and boginess. I set up a PTAGIS folder for downoading processes. Global.R should be reduced as much as possible to just reading in data. This should help with speed, i.e., downloading and massaging only happening upon request or nightly if we go that route.

### Data conventions

We should consider saving data objects in the most primitve class we can get away with to optimize for speed. It should be easy to impiment now, but difficult if we wish it down the road after our code becomes complicated. We should also discuss saving these objects as an RData file vs. csv files or something.

### Workflow

I looked into Github branches and don't think it makes sense for us yet. The work on our lappers is essentially our own branch. The trick will be - to not be working on the same thing at the same time. Maybe we need a "check-in" board on this readme.md to ensure we're not working on the same file concurrently. If so, workflow could look like this:

1. Pull from remote repository to incorporate collaborator's changes
2. Read README.md to see collaborator's notes and check-in board
3. Edit README.md to sign in and state which file(s) you're woking on
4. Commit README.md and push to remote
5. Do work
6. Edit README - sign-off
7. Commit and push



