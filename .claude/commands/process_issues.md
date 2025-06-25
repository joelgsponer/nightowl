# GitHub Issue Processing Workflow
The code snippets are for guidance. You are free to use any tools at your disposal.

## Overview
This workflow automates the complete lifecycle of processing GitHub issues from assignment to completion.

## Prerequisites
- GitHub CLI (`gh`) installed and authenticated
- Git worktree support
- Access to the repository and its project board

## Steps

### 1. Get Issue Details
Fetch issue information from GitHub:

RUN gh issue view "$ISSUE_NUMBER" --json title,body,assignees,labels,state

### 2. Update Local Repository
Switch to dev branch and pull latest changes:

RUN git checkout dev
RUN git pull origin dev

### 3. Analyze Codebase
Search for relevant files and understand the structure:

# Check recent changes in related areas
RUN git log --oneline -n 20s

### 4. Deep Analysis
Perform thorough analysis of the problem:

##### Export issue details for analysis
gh issue view "$ISSUE_NUMBER" --json title,body > /tmp/issue_$ISSUE_NUMBER.json

##### Analyze code patterns and dependencies
echo "Analyzing issue #$ISSUE_NUMBER: $(jq -r .title /tmp/issue_$ISSUE_NUMBER.json)"

### 5. Search Documentation
Search for relevant documentation if needed:

#### Search official docs based on issue context

### 6. Formulate Implementation Plan
Create a structured plan:
PLAN="## Implementation Plan for #$ISSUE_NUMBER

### Analysis
- Problem: [Describe the core issue]
- Root cause: [Identify why this occurs]
- Impact: [Who/what is affected]

### Solution Approach
1. [Step 1 description]
2. [Step 2 description]
3. [Step 3 description]

### Files to Modify
- \`path/to/file1.ext\` - [Why this file needs changes]
- \`path/to/file2.ext\` - [Why this file needs changes]

### Testing Strategy
- [ ] Unit tests for [component]
- [ ] Integration tests for [feature]
- [ ] Manual testing steps


### 7. Add Plan to Issue
Post the plan as a comment on the issue:
RUN gh issue comment "$ISSUE_NUMBER" --body "$PLAN"

### 8. Add Labels
Check for available issue labels.
Add appropriate labels to the issue, create them if they don't exist.

### 9. Update Project Status
List available github projects.
Add issue to the project that seems most fitting.
Change issue status to "In Progress":

### 10. Create Worktree Branch
Set up a new worktree for the feature:

#### Create worktree directory if it doesn't exist
RUN mkdir -p ./tree

#### Generate branch name
BRANCH_NAME="issue-$ISSUE_NUMBER-$(echo "$ISSUE_TITLE" | tr '[:upper:]' '[:lower:]' | tr ' ' '-' | cut -c1-30)"

#### Create and checkout worktree
git worktree add "./tree/$BRANCH_NAME" -b "$BRANCH_NAME" origin/dev
cd "./tree/$BRANCH_NAME"

### 11. Reference Issue in Commits
Ensure all commits reference the issue:

#### Git commit template
GIT_COMMIT_TEMPLATE="fix: [Brief description]

Resolves #$ISSUE_NUMBER

- [Detailed change 1]
- [Detailed change 2]
- [Detailed change 3]"

#### Set up commit hook
echo "#!/bin/bash
if ! grep -q '#$ISSUE_NUMBER' \"\$1\"; then
    echo \"\" >> \"\$1\"
    echo \"Refs #$ISSUE_NUMBER\" >> \"\$1\"
fi" > .git/hooks/prepare-commit-msg
chmod +x .git/hooks/prepare-commit-msg

### 12. Implement the Plan
Execute the implementation:
Add frequent commits, so it can be read like a changelog.

#### Run tests
e.g. npm test || cargo test || pytest
fix any failed tests. 

#### Commit changes
git add -A

### 13. Create Pull Request
Pull and merge dev branch.
Resolve merge conflicts.

#### Push branch
git push -u origin "$BRANCH_NAME"

#### Create PR
Create PR pointing to dev branch:

<example>
gh pr create \
  --base dev \
  --head "$BRANCH_NAME" \
  --title "Fix: $ISSUE_TITLE (#$ISSUE_NUMBER)" \
  --body "## Summary
This PR resolves #$ISSUE_NUMBER

Changes
- [List of changes]

Testing
- [ ] All tests pass
- [ ] Manual testing completed
- [ ] Documentation updated

Screenshots
[If applicable]

Closes #$ISSUE_NUMBER"
</example>

### 14. Update Project Status to Review
Change issue status to "In Review" in the project
Add label "done" to github issue.
Remove label "in-progress"

### 16. Return to Main Branch
Clean up and return to dev:
# Go back to main repository
RUN cd ../..

# Remove worktree after PR is merged
RUN git worktree remove "./tree/$BRANCH_NAME"

# Checkout dev
RUN git checkout dev
RUN git pull origin dev

## Tips
- Keep commits focused and reference the issue
- Update the project board status at each stage
- Test thoroughly before creating the PR
