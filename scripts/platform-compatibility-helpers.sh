#!/bin/bash
# Platform Compatibility Helpers for Typus Project
# Provides fallback implementations for platform-specific commands

# Function to get available memory with fallback
get_available_memory() {
    local default_memory=${1:-512}
    
    if command -v free >/dev/null 2>&1; then
        # Use free command if available
        free -m | awk 'NR==2{printf "%.0f", $7}' 2>/dev/null || echo "$default_memory"
    elif [ -f "/proc/meminfo" ]; then
        # Parse /proc/meminfo as fallback
        local available_kb=$(grep "MemAvailable" /proc/meminfo | awk '{print $2}')
        if [ -n "$available_kb" ]; then
            echo "$((available_kb / 1024))"
        else
            echo "$default_memory"
        fi
    else
        echo "$default_memory"
    fi
}

# Function to run timing with fallback
time_command() {
    local cmd="$1"
    local description="${2:-Command}"
    
    if command -v /usr/bin/time >/dev/null 2>&1; then
        echo "=== Timing $description ==="
        /usr/bin/time -v bash -c "$cmd"
    elif command -v time >/dev/null 2>&1; then
        echo "=== Timing $description ==="
        time bash -c "$cmd"
    else
        echo "=== Running $description (no timing available) ==="
        bash -c "$cmd"
    fi
}

# Function to perform calculations with fallback
calculate() {
    local expression="$1"
    
    if command -v bc >/dev/null 2>&1; then
        echo "$expression" | bc -l
    elif command -v awk >/dev/null 2>&1; then
        # Limited calculation support with awk
        awk "BEGIN {print $expression}"
    elif command -v python3 >/dev/null 2>&1; then
        python3 -c "print($expression)"
    else
        # Very basic integer arithmetic fallback
        echo "$((expression))" 2>/dev/null || echo "0"
    fi
}

# Function to compare floating point numbers
compare_floats() {
    local a="$1"
    local b="$2"
    local operator="$3"
    
    if command -v bc >/dev/null 2>&1; then
        # Use bc for precise floating point comparison
        result=$(echo "$a $operator $b" | bc -l)
        [ "$result" = "1" ]
    elif command -v awk >/dev/null 2>&1; then
        # Use awk for comparison
        awk "BEGIN {exit !($a $operator $b)}"
    else
        # Basic integer comparison fallback
        local int_a=$(printf "%.0f" "$a")
        local int_b=$(printf "%.0f" "$b")
        case "$operator" in
            ">") [ "$int_a" -gt "$int_b" ] ;;
            ">=") [ "$int_a" -ge "$int_b" ] ;;
            "<") [ "$int_a" -lt "$int_b" ] ;;
            "<=") [ "$int_a" -le "$int_b" ] ;;
            "==") [ "$int_a" -eq "$int_b" ] ;;
            "!=") [ "$int_a" -ne "$int_b" ] ;;
            *) false ;;
        esac
    fi
}

# Function to set locale with fallback
set_safe_locale() {
    # Try different locale options in order of preference
    if locale -a | grep -q "^C\.utf8$"; then
        export LC_ALL=C.utf8
        export LANG=C.utf8
    elif locale -a | grep -q "^C\.UTF-8$"; then
        export LC_ALL=C.UTF-8
        export LANG=C.UTF-8
    elif locale -a | grep -q "^en_US\.UTF-8$"; then
        export LC_ALL=en_US.UTF-8
        export LANG=en_US.UTF-8
    else
        # Fallback to basic C locale
        export LC_ALL=C
        export LANG=C
    fi
    
    # Additional locale settings for consistency
    export LC_CTYPE=C
    export LC_MESSAGES=C
    export LC_COLLATE=C
    export LANGUAGE=en_US:en
}

# Function to check if running in CI environment
detect_ci_environment() {
    if [ "$CI" = "true" ] || [ "$GITHUB_ACTIONS" = "true" ] || [ "$GITLAB_CI" = "true" ]; then
        echo "ci"
    elif [ -f /.dockerenv ]; then
        echo "docker"
    else
        echo "local"
    fi
}

# Function to get system architecture
get_system_arch() {
    if command -v uname >/dev/null 2>&1; then
        uname -m
    else
        echo "unknown"
    fi
}

# Function to get OS information
get_os_info() {
    if command -v uname >/dev/null 2>&1; then
        uname -s
    else
        echo "unknown"
    fi
}

# Function to check if command is available with fallback message
check_command_with_fallback() {
    local cmd="$1"
    local fallback_msg="${2:-Command not available}"
    
    if command -v "$cmd" >/dev/null 2>&1; then
        return 0
    else
        echo "$fallback_msg"
        return 1
    fi
}

# Function to normalize file paths for cross-platform compatibility
normalize_path() {
    local path="$1"
    # Convert to absolute path and resolve symlinks
    if command -v realpath >/dev/null 2>&1; then
        realpath "$path"
    elif command -v readlink >/dev/null 2>&1; then
        readlink -f "$path"
    else
        # Basic fallback - just echo the path
        echo "$path"
    fi
}

# Function to create directory with proper permissions
safe_mkdir() {
    local dir="$1"
    if [ ! -d "$dir" ]; then
        mkdir -p "$dir"
        # Set reasonable permissions
        chmod 755 "$dir" 2>/dev/null || true
    fi
}

# Function to check if we're running as root
is_root() {
    [ "$(id -u)" -eq 0 ]
}

# Function to get temporary directory with fallback
get_temp_dir() {
    if [ -n "$TMPDIR" ] && [ -d "$TMPDIR" ]; then
        echo "$TMPDIR"
    elif [ -d "/tmp" ]; then
        echo "/tmp"
    else
        echo "."
    fi
}

# Source this file in other scripts to use these functions
# Example usage:
# source "$(dirname "$0")/platform-compatibility-helpers.sh"
# available_mem=$(get_available_memory 512)
# time_command "cabal build" "Build"
# result=$(calculate "3.14 * 2")
# set_safe_locale