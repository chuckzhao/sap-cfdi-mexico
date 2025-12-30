# Contributing to SAP CFDI Mexico Implementation

Thank you for your interest in contributing to this project! We welcome contributions from the SAP and CFDI community.

## Table of Contents

- [Code of Conduct](#code-of-conduct)
- [How Can I Contribute?](#how-can-i-contribute)
- [Development Setup](#development-setup)
- [Coding Standards](#coding-standards)
- [Commit Guidelines](#commit-guidelines)
- [Pull Request Process](#pull-request-process)
- [Testing Guidelines](#testing-guidelines)
- [Documentation](#documentation)

## Code of Conduct

### Our Pledge

We are committed to providing a welcoming and inclusive environment for all contributors, regardless of experience level, background, or identity.

### Expected Behavior

- Be respectful and inclusive
- Welcome newcomers and help them learn
- Give and receive constructive feedback gracefully
- Focus on what is best for the community
- Show empathy towards other community members

### Unacceptable Behavior

- Harassment or discrimination of any kind
- Trolling, insulting, or derogatory comments
- Publishing others' private information
- Other conduct that could reasonably be considered inappropriate

## How Can I Contribute?

### Reporting Bugs

Before submitting a bug report:
1. Check the [FAQ](docs/05-faq/README.md) for common issues
2. Search existing [GitHub Issues](../../issues) to avoid duplicates
3. Verify the bug in the latest version

When creating a bug report, include:
- Clear, descriptive title
- Steps to reproduce the issue
- Expected vs. actual behavior
- SAP system version (ECC/S/4HANA)
- SAP Basis version
- ABAP stack details
- Error messages or logs
- Screenshots if applicable

### Suggesting Enhancements

Enhancement suggestions are tracked as GitHub Issues. When creating an enhancement suggestion:
- Use a clear, descriptive title
- Provide detailed explanation of the suggested feature
- Explain why this enhancement would be useful
- Include examples of how it would work
- List any alternative solutions considered

### Contributing Code

We accept contributions in several areas:

#### ABAP Code
- New CFDI scenarios
- Performance improvements
- Bug fixes
- Additional PAC integrations
- Enhanced error handling

#### Documentation
- Tutorial improvements
- New scenario documentation
- Translation (Spanish/English)
- Code examples
- Troubleshooting guides

#### Testing
- Unit tests
- Integration tests
- Test data and scenarios
- PAC integration tests

## Development Setup

### Prerequisites

1. **SAP System Access**
   - Development system (DEV)
   - Quality system (QA) for testing
   - ABAP development authorization

2. **Development Tools**
   - SAP GUI or SAP Business Application Studio
   - Git client
   - Text editor for documentation

3. **Knowledge Requirements**
   - ABAP programming
   - SAP SD module
   - CFDI 4.0 specifications
   - Git version control

### Setting Up Your Environment

1. **Fork and Clone**
   ```bash
   git fork https://github.com/original/sap-cfdi-mexico.git
   git clone https://github.com/yourusername/sap-cfdi-mexico.git
   cd sap-cfdi-mexico
   ```

2. **Create a Branch**
   ```bash
   git checkout -b feature/your-feature-name
   ```

3. **Set Up SAP Development**
   - Create a development package in your SAP system
   - Import existing ABAP objects
   - Create a transport request for your changes

## Coding Standards

### ABAP Standards

#### Naming Conventions

Follow SAP standard naming conventions:

```abap
" Classes
CLASS zcl_cfdi_<purpose> DEFINITION.

" Methods
METHOD generate_xml.

" Variables
DATA: lv_variable  TYPE string,      " Local variable
      gv_variable  TYPE string,      " Global variable
      mv_variable  TYPE string,      " Member variable (attribute)
      lt_table     TYPE ty_table,    " Local table
      ls_structure TYPE ty_structure. " Local structure

" Constants
CONSTANTS: lc_version TYPE string VALUE '4.0'.
```

#### Code Style

```abap
" Use meaningful names
METHOD calculate_total_with_taxes.  " Good
METHOD calc.                        " Bad

" Add comments for complex logic
" Calculate taxes according to SAT regulation Article 123
lv_tax = lv_base * lc_tax_rate.

" Use proper indentation (2 spaces)
IF lv_condition = abap_true.
  PERFORM process_data.
  IF lv_nested = abap_true.
    PERFORM nested_process.
  ENDIF.
ENDIF.

" Use structured exception handling
TRY.
    lo_generator->generate_xml( iv_vbeln = lv_vbeln ).
  CATCH cx_cfdi_generation INTO DATA(lx_error).
    MESSAGE lx_error->get_text( ) TYPE 'E'.
ENDTRY.
```

#### Performance Best Practices

```abap
" Use field symbols for table operations
LOOP AT lt_items ASSIGNING FIELD-SYMBOL(<fs_item>).
  <fs_item>-processed = abap_true.
ENDLOOP.

" Avoid nested selects
SELECT * FROM vbrk
  INTO TABLE @lt_billing
  WHERE vbeln IN @lt_doc_numbers.

" Use SELECT SINGLE for single records
SELECT SINGLE * FROM vbrk
  INTO @ls_billing
  WHERE vbeln = @lv_vbeln.
```

### Documentation Standards

#### ABAP Documentation

```abap
"! CFDI Generator Class
"!
"! This class generates CFDI 4.0 XML documents from SAP billing documents
"! according to SAT specifications and sends them to PAC for stamping.
"!
"! @author Your Name | @date 2024-12-27
CLASS zcl_cfdi_generator DEFINITION.

  PUBLIC SECTION.
    "! Generate CFDI XML from billing document
    "!
    "! @parameter iv_vbeln | Billing document number
    "! @parameter iv_version | CFDI version (default 4.0)
    "! @parameter rv_xml | Generated CFDI XML string
    "! @raising cx_cfdi_generation | Generation errors
    METHODS generate_from_billing_doc
      IMPORTING
        iv_vbeln          TYPE vbrk-vbeln
        iv_version        TYPE string DEFAULT '4.0'
      RETURNING
        VALUE(rv_xml)     TYPE string
      RAISING
        cx_cfdi_generation.
```

#### Markdown Documentation

- Use clear headings and structure
- Include code examples
- Add diagrams where helpful
- Keep language clear and concise
- Include both Spanish and English when possible

## Commit Guidelines

### Commit Message Format

```
<type>(<scope>): <subject>

<body>

<footer>
```

### Type

- **feat**: New feature
- **fix**: Bug fix
- **docs**: Documentation changes
- **style**: Code style changes (formatting, no logic change)
- **refactor**: Code refactoring
- **test**: Adding or updating tests
- **chore**: Maintenance tasks

### Examples

```
feat(generator): Add support for payment complement

Implement payment complement (Complemento de Pago) generation
according to CFDI 4.0 specifications. Includes validation
for payment method and date.

Closes #123
```

```
fix(validation): Correct postal code validation logic

Fixed issue where valid postal codes from SAT catalog
were being rejected due to incorrect regex pattern.

Fixes #456
```

```
docs(troubleshooting): Add section on certificate errors

Added troubleshooting steps for common certificate-related
errors including expired certificates and certificate chain issues.
```

## Pull Request Process

### Before Submitting

1. **Test Your Changes**
   - Test in SAP development system
   - Verify all scenarios still work
   - Run unit tests if available
   - Test with different billing document types

2. **Update Documentation**
   - Update README.md if needed
   - Add/update code comments
   - Update relevant docs in docs/ folder
   - Add examples if introducing new features

3. **Code Quality**
   - Follow ABAP coding standards
   - Remove debug statements
   - Clean up commented code
   - Ensure proper error handling

### Submitting the Pull Request

1. **Push Your Changes**
   ```bash
   git push origin feature/your-feature-name
   ```

2. **Create Pull Request**
   - Use a clear, descriptive title
   - Reference related issues
   - Describe what changed and why
   - Include screenshots if relevant
   - Mark as draft if not ready for review

3. **PR Description Template**
   ```markdown
   ## Description
   Brief description of changes

   ## Type of Change
   - [ ] Bug fix
   - [ ] New feature
   - [ ] Documentation update
   - [ ] Performance improvement

   ## Testing
   - Tested in: [SAP System Version]
   - Test scenarios: [List scenarios tested]

   ## Checklist
   - [ ] Code follows project standards
   - [ ] Documentation updated
   - [ ] Tests added/updated
   - [ ] All tests passing
   ```

### Review Process

1. At least one maintainer will review your PR
2. Address review comments
3. Once approved, a maintainer will merge your PR
4. Your contribution will be included in the next release

## Testing Guidelines

### Unit Testing

Create unit tests for new ABAP classes:

```abap
CLASS ltc_cfdi_generator DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA: mo_cut TYPE REF TO zcl_cfdi_generator.

    METHODS:
      setup,
      test_generate_xml FOR TESTING,
      test_validation FOR TESTING,
      test_error_handling FOR TESTING.

ENDCLASS.

CLASS ltc_cfdi_generator IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_cut.
  ENDMETHOD.

  METHOD test_generate_xml.
    DATA(lv_xml) = mo_cut->generate_from_billing_doc(
      iv_vbeln = '0090000001'
    ).
    cl_abap_unit_assert=>assert_not_initial(
      act = lv_xml
      msg = 'XML should not be empty'
    ).
  ENDMETHOD.

ENDCLASS.
```

### Integration Testing

Test with real SAP data:
- Create test billing documents
- Test complete flow (generation → stamping → storage)
- Verify with different document types
- Test error scenarios

### Test Data

Do not commit:
- Production data
- Real certificates
- Actual credentials
- Customer information

Use only:
- SAT test credentials
- Test certificates
- Anonymized data
- Sample documents

## Documentation

### Required Documentation for New Features

1. **Code Comments**
   - Explain complex logic
   - Document assumptions
   - Reference SAT regulations

2. **User Documentation**
   - Add to appropriate docs/ section
   - Include examples
   - Update table of contents

3. **API Documentation**
   - Document all public methods
   - Include parameter descriptions
   - Provide usage examples

### Documentation Style

- Write clearly and concisely
- Use active voice
- Include practical examples
- Add diagrams for complex processes
- Provide both Spanish and English when possible

## Questions?

If you have questions about contributing:
- Check the [FAQ](docs/05-faq/README.md)
- Open a [Discussion](../../discussions)
- Reach out to maintainers

## Recognition

Contributors will be recognized in:
- CHANGELOG.md for their contributions
- GitHub contributors page
- Special thanks section for significant contributions

Thank you for contributing to SAP CFDI Mexico Implementation!
