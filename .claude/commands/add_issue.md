You are an AI assistant tasked with creating well-structured GitHub issues for
feature requests, bug reports, or improvement ideas. Your goal is to turn the
provided feature description into a comprehensive GitHub issue that follows best
practices and project conventions.

Use parallel running subagents whenever possible.

First, you will be given a feature description:

<feature_description>
# $ARGUMENTS
</feature_description>

Follow these steps to complete the task, make a todo list and think ultrahard:

1. Research the repository:
   - Visit the examine the repository's structure, existing
     issues, and documentation.
   - Look for any CONTRIBUTING.md, ISSUE_TEMPLATE.md, or similar files that might
     contain guidelines for creating issues.
   - Note the project's coding style, naming conventions, and any specific
     requirements for submitting issues.

2. Best practices:
   - Apply best practices for writing GitHub Issues.

3. Present a plan:
   - Based on your research, outline a plan for creating the GitHub issue.
   - Include the proposed structure of the issue, any labels or milestones you plan
     to use, and how you'll incorporate project-specific conventions.
   - Present this plan in <plan> tags.

4. Create the GitHub issue:
   - Once the plan is approved, draft the GitHub issue content.
   - Include a clear title, detailed description, acceptance criteria, and any
     additional context or resources that would be helpful for developers.
   - Use appropriate formatting (e.g., Markdown) to enhance readability.
   - Add any relevant labels, milestones, or assignees based on the project's
     conventions.
   - use the `gh` cli tool to do so.
   - Do not proceed to write any code only the github issue until explicitly told otherwise
   - Add the issue to the repos project page (e.g. `gh project item-add`).

