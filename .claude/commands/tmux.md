# Tmux Development Environment Setup Workflow

## Overview
This workflow sets up a tmux development environment with multiple panes and virtual environment activation.

## Steps

### 1. Set Tmux Pane Name
Set the current tmux pane name to a provided argument:
```bash
tmux rename-window "${ARGUMENT}"
```

### 2. Check for Virtual Environment
Check if a virtual environment exists in common locations:
```bash
if [ -d "venv" ]; then
    VENV_PATH="venv"
elif [ -d ".venv" ]; then
    VENV_PATH=".venv"
elif [ -d "env" ]; then
    VENV_PATH="env"
elif [ -f "Pipfile" ]; then
    echo "Pipenv project detected"
    VENV_PATH="pipenv"
else
    echo "No virtual environment found"
    VENV_PATH=""
fi
```

### 3. Create Left Pane
Split the window horizontally to create a pane on the left:
```bash
tmux split-window -h -p 40
```
This creates a new pane taking 40% of the window width.

### 4. Create Bottom Pane
Select the main pane and split vertically to create a bottom pane:
```bash
tmux select-pane -t 0
tmux split-window -v -p 30
```
This creates a bottom pane taking 30% of the height.

### 5. Source Virtual Environment (Main Pane)
Activate the virtual environment in the current pane:
```bash
if [ -n "$VENV_PATH" ]; then
    if [ "$VENV_PATH" = "pipenv" ]; then
        pipenv shell
    else
        source "$VENV_PATH/bin/activate"
    fi
fi
```

### 6. Source Virtual Environment in Left Pane
Send commands to activate the virtual environment in the left pane:
```bash
if [ -n "$VENV_PATH" ]; then
    if [ "$VENV_PATH" = "pipenv" ]; then
        tmux send-keys -t 1 "pipenv shell" C-m
    else
        tmux send-keys -t 1 "source $VENV_PATH/bin/activate" C-m
    fi
fi
```

### 7. Source Virtual Environment in Bottom Pane
Send commands to activate the virtual environment in the bottom pane:
```bash
if [ -n "$VENV_PATH" ]; then
    if [ "$VENV_PATH" = "pipenv" ]; then
        tmux send-keys -t 2 "pipenv shell" C-m
    else
        tmux send-keys -t 2 "source $VENV_PATH/bin/activate" C-m
    fi
fi
```
RUN THIS WORKFLOW
