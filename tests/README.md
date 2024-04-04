<link rel="shortcut icon" type="image/png" href="../logo/3.favicon_radius.png">

# Overview of the test projects

We have two test projects you can use when writing your tests:

1. - Casestudy: `test`
   - Selection process: `testing`
   - Contains the following data:
     * Commits
     * Mails
     * Issues
     * Authors
     * Bots
     * Gender
     * Commit messages
     * Pasta
     * Synchronicity
     * Commit Interactions
     * Custom event timestamps in `custom-events.list`
     * Revisions
2. - Casestudy: `test_empty`
   - Selection process: `testing`
   - Contains the following data:
     * Authors
     * Revisions

Please note that all projects must have author and revision data as otherwise, `coronet` stops when reading the data.
Everything else can be empty.
