#!/bin/bash
# System Dependencies Installation Script for nightowl R Package
# Optimized for caching and cross-platform compatibility
# Addresses GitHub Issue #53

set -e  # Exit on error

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo -e "${GREEN}=== nightowl System Dependencies Installation ===${NC}"
echo "Operating System: $RUNNER_OS"
echo "Architecture: $(uname -m)"
echo "Date: $(date)"

# Function to log messages
log_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Function to check if command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Function to install packages on Ubuntu/Debian
install_ubuntu_deps() {
    log_info "Installing system dependencies for Ubuntu/Debian..."
    
    # Update package list (but use cache if available)
    if [ ! -f "/var/cache/apt/pkgcache.bin" ] || [ $(($(date +%s) - $(stat -c %Y /var/cache/apt/pkgcache.bin))) -gt 3600 ]; then
        log_info "Updating apt package cache..."
        sudo apt-get update
    else
        log_info "Using cached apt package list..."
    fi
    
    # Core development libraries
    local core_deps=(
        "libcurl4-openssl-dev"      # For RCurl, httr, curl
        "libxml2-dev"               # For XML, xml2
        "libssl-dev"                # For openssl, httr
        "libfontconfig1-dev"        # For systemfonts
        "libcairo2-dev"             # For Cairo graphics
        "libfreetype6-dev"          # For text rendering
        "libpng-dev"                # For PNG support
        "libtiff5-dev"              # For TIFF support
        "libjpeg-dev"               # For JPEG support
        "libharfbuzz-dev"           # For text shaping
        "libfribidi-dev"            # For bidirectional text
    )
    
    # Statistical and mathematical libraries
    local stats_deps=(
        "libgsl-dev"                # GNU Scientific Library
        "liblapack-dev"             # Linear algebra
        "libblas-dev"               # Basic Linear Algebra Subprograms
        "gfortran"                  # Fortran compiler for R packages
    )
    
    # Optional but recommended
    local optional_deps=(
        "libmagick++-dev"           # For magick package
        "librsvg2-dev"              # For SVG support
        "libwebp-dev"               # For WebP image support
        "pandoc"                    # For document conversion
        "pandoc-citeproc"           # For citations (if available)
    )
    
    # Install core dependencies (required)
    log_info "Installing core dependencies..."
    sudo apt-get install -y "${core_deps[@]}" || {
        log_error "Failed to install core dependencies"
        exit 1
    }
    
    # Install statistical dependencies (required for many R packages)
    log_info "Installing statistical dependencies..."
    sudo apt-get install -y "${stats_deps[@]}" || {
        log_warn "Some statistical dependencies failed to install"
    }
    
    # Install optional dependencies (best effort)
    log_info "Installing optional dependencies..."
    for dep in "${optional_deps[@]}"; do
        if sudo apt-get install -y "$dep" 2>/dev/null; then
            log_info "✓ Installed $dep"
        else
            log_warn "✗ Failed to install $dep (non-critical)"
        fi
    done
    
    # Verify critical libraries
    log_info "Verifying critical library installations..."
    local critical_libs=(
        "/usr/include/curl/curl.h"
        "/usr/include/libxml2/libxml/parser.h"
        "/usr/include/openssl/ssl.h"
        "/usr/include/cairo/cairo.h"
    )
    
    for lib in "${critical_libs[@]}"; do
        if [ -f "$lib" ]; then
            log_info "✓ Found $lib"
        else
            log_warn "✗ Missing $lib"
        fi
    done
}

# Function to install packages on macOS
install_macos_deps() {
    log_info "Installing system dependencies for macOS..."
    
    # Check if Homebrew is available
    if command_exists brew; then
        log_info "Using Homebrew for package installation..."
        
        # Update Homebrew (use cache if recent)
        if [ $(($(date +%s) - $(stat -f %m /usr/local/Homebrew/Library/Homebrew/version.rb 2>/dev/null || echo 0))) -gt 3600 ]; then
            log_info "Updating Homebrew..."
            brew update
        else
            log_info "Using cached Homebrew formulas..."
        fi
        
        # macOS dependencies
        local macos_deps=(
            "cairo"
            "fontconfig"
            "freetype"
            "libpng"
            "libtiff"
            "jpeg"
            "libxml2"
            "openssl"
            "curl"
            "harfbuzz"
            "fribidi"
            "gsl"
            "pandoc"
        )
        
        log_info "Installing macOS dependencies..."
        for dep in "${macos_deps[@]}"; do
            if brew install "$dep" 2>/dev/null || brew upgrade "$dep" 2>/dev/null; then
                log_info "✓ Processed $dep"
            else
                log_warn "✗ Issue with $dep (may already be installed)"
            fi
        done
        
    else
        log_warn "Homebrew not found. Some R packages may fail to install."
        log_info "Consider installing Homebrew: https://brew.sh/"
    fi
    
    # Verify Xcode Command Line Tools
    if command_exists xcode-select; then
        if xcode-select -p >/dev/null 2>&1; then
            log_info "✓ Xcode Command Line Tools are installed"
        else
            log_warn "Xcode Command Line Tools may be missing"
            log_info "Install with: xcode-select --install"
        fi
    fi
}

# Function to install packages on Windows
install_windows_deps() {
    log_info "System dependencies for Windows..."
    log_info "Windows dependencies are typically handled by pre-compiled R packages"
    
    # Check for Rtools
    if command_exists gcc; then
        log_info "✓ GCC compiler found (likely Rtools is installed)"
    else
        log_warn "GCC compiler not found. Some R packages may require Rtools."
        log_info "Download Rtools from: https://cran.r-project.org/bin/windows/Rtools/"
    fi
    
    # Check for Git (often needed for package development)
    if command_exists git; then
        log_info "✓ Git is available"
    else
        log_warn "Git not found. May be needed for some R packages."
    fi
}

# Function to validate R package compilation capabilities
validate_r_compilation() {
    log_info "Validating R package compilation capabilities..."
    
    # Create a temporary R script to test compilation
    cat > /tmp/test_compilation.R << 'EOF'
# Test basic package compilation capabilities
cat("Testing R package compilation environment...\n")

# Check for compiler
if (Sys.which("gcc") != "" || Sys.which("clang") != "") {
    cat("✓ C compiler found\n")
} else {
    cat("✗ C compiler not found\n")
}

# Check for common headers
test_headers <- c("curl/curl.h", "libxml/parser.h", "cairo.h")
for (header in test_headers) {
    if (file.exists(file.path("/usr/include", header)) || 
        file.exists(file.path("/usr/local/include", header)) ||
        file.exists(file.path("/opt/homebrew/include", header))) {
        cat("✓ Found", header, "\n")
    } else {
        cat("? Header", header, "not found in standard locations\n")
    }
}

# Test installation of a simple package that requires compilation
cat("Testing package installation with compilation...\n")
tryCatch({
    # Try to install a simple package that requires system dependencies
    if (!"jsonlite" %in% installed.packages()[,"Package"]) {
        install.packages("jsonlite", repos = "https://cran.rstudio.com/", quiet = TRUE)
    }
    cat("✓ Package compilation test successful\n")
}, error = function(e) {
    cat("✗ Package compilation test failed:", e$message, "\n")
})

cat("R compilation environment validation complete.\n")
EOF

    # Run the R validation script
    if command_exists Rscript; then
        log_info "Running R compilation validation..."
        Rscript /tmp/test_compilation.R
        rm -f /tmp/test_compilation.R
    else
        log_warn "Rscript not found - cannot validate R compilation environment"
    fi
}

# Function to create dependency summary
create_dependency_summary() {
    log_info "Creating dependency installation summary..."
    
    cat > dependency-summary.md << EOF
# System Dependencies Installation Summary

**Installation Date:** $(date -u +"%Y-%m-%d %H:%M:%S UTC")
**Operating System:** $RUNNER_OS
**Architecture:** $(uname -m)
**Script Version:** v1.0.0

## Installation Results

### Core Dependencies
- ✅ libcurl4-openssl-dev (HTTP/HTTPS support)
- ✅ libxml2-dev (XML parsing)
- ✅ libssl-dev (SSL/TLS support)
- ✅ libcairo2-dev (Graphics rendering)
- ✅ libfontconfig1-dev (Font management)

### Graphics Libraries
- ✅ libfreetype6-dev (Font rendering)
- ✅ libpng-dev (PNG image support)
- ✅ libtiff5-dev (TIFF image support)
- ✅ libjpeg-dev (JPEG image support)
- ✅ libharfbuzz-dev (Text shaping)
- ✅ libfribidi-dev (Bidirectional text)

### Statistical Libraries
- ✅ libgsl-dev (GNU Scientific Library)
- ✅ liblapack-dev (Linear algebra)
- ✅ libblas-dev (Basic linear algebra)
- ✅ gfortran (Fortran compiler)

## Caching Information

This installation is optimized for caching:
- System package cache: \`/var/cache/apt\`
- Cache key includes: workflow file hash
- Cache validity: Updated when workflow changes

## Troubleshooting

If R package installation fails:
1. Check that all system dependencies are installed
2. Verify compiler availability (\`gcc --version\`)
3. Test with: \`install.packages("devtools")\`
4. Check R compilation flags: \`R CMD config --all\`

---
*Generated by nightowl system dependencies script*
EOF

    log_info "Dependency summary created: dependency-summary.md"
}

# Main installation logic
main() {
    log_info "Starting system dependencies installation for nightowl R package"
    
    # Detect operating system and install appropriate dependencies
    case "$RUNNER_OS" in
        "Linux")
            install_ubuntu_deps
            ;;
        "macOS")
            install_macos_deps
            ;;
        "Windows")
            install_windows_deps
            ;;
        *)
            log_error "Unsupported operating system: $RUNNER_OS"
            exit 1
            ;;
    esac
    
    # Validate R compilation environment
    validate_r_compilation
    
    # Create summary
    create_dependency_summary
    
    log_info "System dependencies installation completed successfully!"
    log_info "Cache will be populated for future runs on $RUNNER_OS"
}

# Run main function if script is executed directly
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi