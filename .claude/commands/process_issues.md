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
```bash
gh issue view "$ISSUE_NUMBER" --json title,body,assignees,labels,state
```

### 2. Update Local Repository
Switch to dev branch and pull latest changes:
```bash
git checkout dev
git pull origin dev
```

### 3. Analyze Codebase
Search for relevant files and understand the structure:
```bash

# Check recent changes in related areas
git log --oneline -n 20s
```

### 4. Deep Analysis
Perform thorough analysis of the problem:
```bash
# Export issue details for analysis
gh issue view "$ISSUE_NUMBER" --json title,body > /tmp/issue_$ISSUE_NUMBER.json

# Analyze code patterns and dependencies
echo "Analyzing issue #$ISSUE_NUMBER: $(jq -r .title /tmp/issue_$ISSUE_NUMBER.json)"
```

### 5. Search Documentation
Search for relevant documentation if needed:
```bash
# Search official docs based on issue context
SEARCH_TERMS=$(jq -r .title /tmp/issue_$ISSUE_NUMBER.json | tr ' ' '+')
echo "Search for: $SEARCH_TERMS in relevant documentation"
```

### 6. Formulate Implementation Plan
Create a structured plan:
```bash
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

### Estimated Time: [X hours]"
```

### 7. Add Plan to Issue
Post the plan as a comment on the issue:
```bash
gh issue comment "$ISSUE_NUMBER" --body "$PLAN"
```

### 8. Add Labels
Add appropriate labels to the issue, creating them if they don't exist:
```bash
# Function to create label if it doesn't exist
create_label_if_missing() {
    local label_name="$1"
    local label_color="$2"
    local label_description="$3"
    
    if ! gh label list --json name -q ".[] | select(.name == \"$label_name\")"; then
        echo "Creating label: $label_name"
        gh label create "$label_name" --color "$label_color" --description "$label_description"
    fi
}

# Ensure required labels exist
create_label_if_missing "in-progress" "fbca04" "Work is currently in progress"
create_label_if_missing "enhancement" "a2eeef" "New feature or request"
create_label_if_missing "bug" "d73a4a" "Something isn't working"
create_label_if_missing "feature" "0075ca" "New feature implementation"

# Add default labels
gh issue edit "$ISSUE_NUMBER" --add-label "in-progress,enhancement"

# Add type-specific labels based on issue title
if [[ "$ISSUE_TITLE" =~ "bug" ]]; then
    gh issue edit "$ISSUE_NUMBER" --add-label "bug"
elif [[ "$ISSUE_TITLE" =~ "feature" ]]; then
    gh issue edit "$ISSUE_NUMBER" --add-label "feature"
fi
```

### 9. Update Project Status
Change issue status to "In Progress":
```bash
# Get project info
PROJECT_ID=$(gh project list --owner "$(gh repo view --json owner -q .owner.login)" --json id,title -q '.[] | select(.title=="Your Project Name") | .id')

# Update status
gh project item-edit --project-id "$PROJECT_ID" --id "$ISSUE_NUMBER" --field-id "Status" --value "In Progress"
```

### 10. Create Worktree Branch
Set up a new worktree for the feature:
```bash
# Create worktree directory if it doesn't exist
mkdir -p ./tree

# Generate branch name
BRANCH_NAME="issue-$ISSUE_NUMBER-$(echo "$ISSUE_TITLE" | tr '[:upper:]' '[:lower:]' | tr ' ' '-' | cut -c1-30)"

# Create and checkout worktree
git worktree add "./tree/$BRANCH_NAME" -b "$BRANCH_NAME" origin/dev
cd "./tree/$BRANCH_NAME"
```

### 11. Reference Issue in Commits
Ensure all commits reference the issue:
```bash
# Git commit template
GIT_COMMIT_TEMPLATE="fix: [Brief description]

Resolves #$ISSUE_NUMBER

- [Detailed change 1]
- [Detailed change 2]
- [Detailed change 3]"

# Set up commit hook
echo "#!/bin/bash
if ! grep -q '#$ISSUE_NUMBER' \"\$1\"; then
    echo \"\" >> \"\$1\"
    echo \"Refs #$ISSUE_NUMBER\" >> \"\$1\"
fi" > .git/hooks/prepare-commit-msg
chmod +x .git/hooks/prepare-commit-msg
```

### 12. Implement the Plan
Execute the implementation:
```bash
# Example implementation workflow
echo "Implementing solution for issue #$ISSUE_NUMBER"

# Make changes according to plan
# ... implementation code ...

# Run tests
npm test || cargo test || pytest

# Commit changes
git add -A
git commit -m "fix: Implement solution for issue #$ISSUE_NUMBER

- Added validation for user input
- Fixed memory leak in data processor
- Updated documentation

Resolves #$ISSUE_NUMBER"
```

### 13. Create Pull Request
Create PR pointing to dev branch:
```bash
# Push branch
git push -u origin "$BRANCH_NAME"

# Create PR
gh pr create \
  --base dev \
  --head "$BRANCH_NAME" \
  --title "Fix: $ISSUE_TITLE (#$ISSUE_NUMBER)" \
  --body "## Summary
This PR resolves #$ISSUE_NUMBER

## Changes
- [List of changes]

## Testing
- [ ] All tests pass
- [ ] Manual testing completed
- [ ] Documentation updated

## Screenshots
[If applicable]

Closes #$ISSUE_NUMBER"
```

### 14. Update Project Status to Review
Change issue status to "In Review":
```bash
gh project item-edit --project-id "$PROJECT_ID" --id "$ISSUE_NUMBER" --field-id "Status" --value "In Review"
```

### 15. Close the Issue
The issue will auto-close when PR is merged, but if needed:
```bash
# Only if manually closing
gh issue close "$ISSUE_NUMBER" --reason completed --comment "Resolved in PR #$(gh pr view --json number -q .number)"
```

### 16. Return to Main Branch
Clean up and return to dev:
```bash
# Go back to main repository
cd ../..

# Remove worktree after PR is merged
git worktree remove "./tree/$BRANCH_NAME"

# Checkout dev
git checkout dev
git pull origin dev
```

## Complete Script
Here's the complete automated script:


## Tips
- Use meaningful branch names that include the issue number
- Keep commits focused and reference the issue
- Update the project board status at each stage
- Test thoroughly before creating the PR
