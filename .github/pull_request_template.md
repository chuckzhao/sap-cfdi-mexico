# Pull Request

## Description

Please provide a brief description of the changes in this PR.

## Type of Change

- [ ] Bug fix (non-breaking change which fixes an issue)
- [ ] New feature (non-breaking change which adds functionality)
- [ ] Breaking change (fix or feature that would cause existing functionality to not work as expected)
- [ ] Documentation update
- [ ] Performance improvement
- [ ] Code refactoring
- [ ] Other (please describe):

## Related Issues

Fixes # (issue number)
Relates to # (issue number)

## Changes Made

Please list the main changes made in this PR:

-
-
-

## CFDI Scenarios Affected

Which CFDI scenarios are affected by this change?

- [ ] Invoice generation
- [ ] Credit notes
- [ ] Debit notes
- [ ] Payment complements
- [ ] Foreign trade
- [ ] Withholding receipts
- [ ] Certificate management
- [ ] PAC integration
- [ ] Other: ___________

## Testing

### Test Environment

- [ ] Tested in SAP Development system
- [ ] Tested in SAP Quality system
- [ ] Tested with PAC test environment
- [ ] Unit tests added/updated
- [ ] Integration tests performed

**SAP Version Tested:** [e.g., ECC 6.0, S/4HANA 2021]
**PAC Provider Tested:** [e.g., Montova, Edicom]

### Test Cases

Describe the testing performed:

1. Test case 1:
   - Input:
   - Expected result:
   - Actual result:

2. Test case 2:
   - Input:
   - Expected result:
   - Actual result:

### Test Results

- [ ] All tests passed
- [ ] Some tests failed (explain below)
- [ ] Not tested (explain why)

## Code Quality

- [ ] Code follows SAP ABAP naming conventions
- [ ] Code follows project coding standards
- [ ] Self-review of code performed
- [ ] Code is properly commented
- [ ] ABAP documentation (ABAP Doc) added/updated
- [ ] No syntax errors or warnings
- [ ] Code Inspector check passed
- [ ] Extended program check passed (SLIN)

## Documentation

- [ ] README.md updated (if needed)
- [ ] Documentation updated in docs/ folder
- [ ] Code comments added/updated
- [ ] Examples added/updated (if applicable)
- [ ] CHANGELOG.md updated

## Breaking Changes

Does this PR introduce breaking changes?

- [ ] Yes (describe below)
- [ ] No

**If yes, describe:**


## Migration Guide

If this introduces breaking changes, provide migration guide:


## Screenshots (if applicable)

Add screenshots to demonstrate changes (especially for UI changes or reports).

## Checklist

### Before Submitting

- [ ] I have read the [CONTRIBUTING](../../CONTRIBUTING.md) guide
- [ ] My code follows the project's coding standards
- [ ] I have performed a self-review of my code
- [ ] I have commented my code, particularly in hard-to-understand areas
- [ ] I have made corresponding changes to the documentation
- [ ] My changes generate no new warnings or errors
- [ ] I have added tests that prove my fix is effective or that my feature works
- [ ] New and existing unit tests pass locally with my changes
- [ ] Any dependent changes have been merged and published

### ABAP Specific

- [ ] All ABAP objects activated
- [ ] Transport request created (if applicable)
- [ ] No hardcoded values (configuration in tables)
- [ ] No database commits in update logic
- [ ] Proper exception handling implemented
- [ ] Authorization checks implemented (where needed)

### PAC Integration

- [ ] Tested with PAC test environment
- [ ] Error handling for PAC failures
- [ ] Retry logic implemented (if applicable)
- [ ] Logging added for debugging

## Additional Notes

Any additional information that reviewers should know:


## Reviewer Notes

For reviewers:
- [ ] Code review completed
- [ ] ABAP syntax check passed
- [ ] Testing verified
- [ ] Documentation adequate
- [ ] Ready to merge

---

**Note to Reviewers:** Please ensure all checkboxes are marked before approving.
