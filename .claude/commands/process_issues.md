# GitHub Issue Processing Workflow
The code snippets are for guidance. You are free to use any tools at your disposal.

## Overview
This workflow automates the complete lifecycle of processing GitHub issues from assignment to completion.

## Prerequisites
- GitHub CLI (`gh`) installed and authenticated
- Git worktree support
- Access to the repository and its project board

## Critical Success Factors from Practice
**Based on real session learnings:**
- **GOLD STANDARD**: Create feature branch and worktree FIRST, then analyze within isolated environment
- **Branch from local dev**: Always branch from locally current dev branch, not origin/dev
- **Autonomous execution required**: Workflow must work correctly without user confirmations or interventions
- **Use Task tool for complex searches**: More effective than manual grep for pattern matching and scope analysis
- **TodoWrite integration essential**: Track all steps and progress transparently
- **Handle working directory state upfront**: Check and resolve local changes before starting any workflow
- **Robust error handling**: Anticipate and handle common issues like config file conflicts automatically

## Steps

### 0. Pre-Flight Checks - CRITICAL FIRST STEP
Before doing ANY analysis, establish clean baseline:

```bash
# Check current working directory state
git status

# Handle any uncommitted changes decisively
# User preference: commit local changes or add to .gitignore
# Common pattern: .claude/ directory conflicts

# Verify current branch and ensure dev is up to date
git checkout dev && git pull origin dev
```
**Why**: Clean working directory prevents conflicts during branch operations and ensures predictable behavior.

### 1. Get Issue Details
Fetch issue information from GitHub:

```bash
# Fetch comprehensive issue information
gh issue view "$ISSUE_NUMBER" --json title,body,assignees,labels,state

# Get just the title for planning
ISSUE_TITLE=$(gh issue view "$ISSUE_NUMBER" --json title --jq '.title')
echo "Working on: $ISSUE_TITLE"
```

### 2. Create TodoWrite Plan - MANDATORY TRACKING
Before any implementation, create comprehensive todo list:

```
TodoWrite with todos like:
- Create feature branch/worktree from local dev
- Analyze issue scope and affected files
- Implement changes in [specific files]
- Update test files and documentation
- Run tests and validate
- Create pull request
```
**Why**: Provides transparency, prevents missed steps, tracks progress

### 3. Create Worktree Branch - GOLD STANDARD APPROACH
Set up isolated environment FIRST, before any analysis:

```bash
# Create worktree directory if it doesn't exist
mkdir -p ./tree

# Generate branch name (ensure it's valid and descriptive)
BRANCH_NAME="issue-$ISSUE_NUMBER-$(echo "$ISSUE_TITLE" | tr '[:upper:]' '[:lower:]' | tr ' ' '-' | sed 's/[^a-z0-9-]//g' | cut -c1-30)"

# Validate branch name doesn't already exist
if git show-ref --verify --quiet refs/heads/"$BRANCH_NAME"; then
    BRANCH_NAME="${BRANCH_NAME}-$(date +%s)"
fi

# CRITICAL: Create from LOCAL dev branch (not origin/dev)
git worktree add "./tree/$BRANCH_NAME" -b "$BRANCH_NAME" dev
cd "./tree/$BRANCH_NAME"
```
**Why**: 
- Immediate isolation prevents conflicts with main branch
- Autonomous branch creation with collision handling prevents workflow interruption
- Branching from local dev ensures current state is preserved
- Sanitized branch names avoid git issues

### 4. Deep Code Analysis - USE TASK TOOL
**CRITICAL**: Use Task tool for complex searches rather than manual grep:

```
Task tool with prompt: "Search for [pattern/keyword] in the codebase to understand scope of issue #X. 
Find all functions/files that need modification and provide specific line numbers and context."
```

**Why Task tool is superior**:
- More intelligent pattern matching
- Better context understanding  
- Reduced false positives
- Comprehensive scope analysis

### 5. Search Documentation
Search for relevant documentation if needed:

#### Search official docs based on issue context

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
```

### 7. Add Plan to Issue
Post the plan as a comment on the issue:
```bash
gh issue comment "$ISSUE_NUMBER" --body "$PLAN"
```

### 8. Add Labels
Check for available issue labels.
Add appropriate labels to the issue, create them if they don't exist.

### 9. Update Project Status
List available github projects.
Add issue to the project that seems most fitting.
Change issue status to "In Progress":

### 10. Reference Issue in Commits
Ensure all commits reference the issue:

#### Git commit template
```bash
GIT_COMMIT_TEMPLATE="fix: [Brief description]

Resolves #$ISSUE_NUMBER

- [Detailed change 1]
- [Detailed change 2]
- [Detailed change 3]"
```

#### Set up commit hook
```bash
echo "#!/bin/bash
if ! grep -q '#$ISSUE_NUMBER' \"\$1\"; then
    echo \"\" >> \"\$1\"
    echo \"Refs #$ISSUE_NUMBER\" >> \"\$1\"
fi" > .git/hooks/prepare-commit-msg
chmod +x .git/hooks/prepare-commit-msg
```

### 11. Implement the Plan
Execute the implementation:
Add frequent commits, so it can be read like a changelog.

#### IMPORTANT: Update Test Files
After changing function signatures, always check and update:
- Test files that call the modified functions
- Example files and vignettes
- Documentation examples

**Pattern**: Search for `.parameter_name =` usage in test files and update to match new signatures.

#### Run tests
```bash
# Use project-specific test command
devtools::test()  # For R packages
# OR npm test || cargo test || pytest
```
Fix any failed tests.

#### Commit changes
```bash
git add -A
git commit -m "implement: [description]

Refs #$ISSUE_NUMBER"
```

### 12. Create Pull Request
Merge latest dev changes and create PR:

#### Merge latest dev changes
```bash
git fetch origin dev
git merge origin/dev
```
**Why**: Stay current with main branch to avoid conflicts and include latest changes.

#### Push branch
```bash
git push -u origin "$BRANCH_NAME"
```

#### Create PR
Create PR pointing to dev branch:

```bash
gh pr create \
  --base dev \
  --head "$BRANCH_NAME" \
  --title "Fix: $ISSUE_TITLE (#$ISSUE_NUMBER)" \
  --body "$(cat <<'EOF'
## Summary
<1-3 bullet points>

## Test plan
[Checklist of TODOs for testing the pull request...]

🤖 Generated with [Claude Code](https://claude.ai/code)
EOF
)"
```

### 13. Update Project Status to Review
Change issue status to "In Review" in the project
Add label "done" to github issue.
Remove label "in-progress"

### 14. Return to Main Branch
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

## Tips
- Keep commits focused and reference the issue
- Update the project board status at each stage
- Test thoroughly before creating the PR

## Session Assessment: Issue #26 "replace .data"

### What This Session Taught Us

**Issue Complexity**: Parameter renaming across 7+ R files plus test files - moderate complexity requiring systematic approach.

**Key Success Factors Validated**:

1. **Task Tool Superiority**: The Task tool's comprehensive search was invaluable for finding all `.data` occurrences. It distinguished between function parameters vs dplyr `.data` pronoun usage - something manual grep would struggle with.

2. **Pre-flight Checks Critical**: Had to handle `.claude/settings.local.json` conflicts before proceeding. This validated the need for the pre-flight step.

3. **TodoWrite Transparency**: The 8-step todo list provided clear progress tracking and ensured no steps were missed.

4. **Test File Updates Essential**: Required updating `test-summary.R` and `test-vignette-getting-started.R` to match new function signatures - this wasn't in the original workflow.

5. **Autonomous Execution**: The workflow completed without user intervention, validating the autonomous design.

**Specific Improvements Incorporated**:

- **Pre-flight checks** (Step 0): Handle dirty working directory upfront
- **Task tool guidance**: When to use vs manual searches  
- **TodoWrite requirement**: Mandatory for multi-step issues
- **Test file consideration**: Explicit step to check/update test files
- **Merge dev step**: Stay current with main branch before PR
- **Enhanced error handling**: Better guidance on common patterns

### Anti-patterns to avoid
- Never start git operations with dirty working directory
- Never proceed with branch creation until scope analysis is complete
- Never assume user will resolve conflicts - build autonomous handling
- Never skip TodoWrite planning for multi-step issues
