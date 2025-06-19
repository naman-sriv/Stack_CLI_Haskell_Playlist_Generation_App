# Stack CLI Haskell Playlist Generation App

This project is a command-line application built in Haskell for generating music playlists. It was developed as part of group coursework for ECS713P - Functional Programming (2022/23).

## Table of Contents

- [Project Overview](#project-overview)
- [Features](#features)
- [Installation](#installation)
- [Usage](#usage)
- [Example Commands](#example-commands)
- [Technologies Used](#technologies-used)
- [Project Structure](#project-structure)
- [Contributing](#contributing)
- [License](#license)

## Project Overview

The **Stack CLI Haskell Playlist Generation App** allows users to create, manage, and manipulate music playlists from the command line. Users can add tracks, remove tracks, generate playlists based on criteria (genre, artist, etc.), and save playlists to files. The project demonstrates practical use of functional programming concepts in Haskell.

## Features

- Add, remove, and list tracks in a playlist
- Generate playlists based on filters (e.g., genre, artist)
- Save and load playlists from files
- Command-line interface for all operations
- Error handling and user-friendly prompts

## Installation

### Prerequisites

- [Haskell Stack](https://docs.haskellstack.org/en/stable/README/)
- GHC (comes with Stack)

### Steps

1. **Clone the repository:**

   ```bash
   git clone https://github.com/naman-sriv/Stack_CLI_Haskell_Playlist_Generation_App.git
   cd Stack_CLI_Haskell_Playlist_Generation_App
   ```

2. **Install dependencies and build:**

   ```bash
   stack setup
   stack build
   ```

3. **Run the application:**

   ```bash
   stack run
   ```

## Usage

After building the project, you can start the CLI app and use various commands to manage playlists.

### Basic Commands

- `add`: Add a new track to the playlist
- `remove`: Remove a track from the playlist
- `list`: List all tracks in the current playlist
- `generate`: Generate a playlist based on specific criteria
- `save`: Save the current playlist to a file
- `load`: Load a playlist from a file
- `help`: Show available commands

## Example Commands

```bash
# Add a new track
add "Bohemian Rhapsody" "Queen" "Rock" 1975

# Remove a track by title
remove "Bohemian Rhapsody"

# List all tracks
list

# Generate a playlist by genre
generate genre "Rock"

# Save playlist to a file
save my_rock_playlist.txt

# Load a playlist from a file
load my_rock_playlist.txt
```

## Technologies Used

- Haskell
- Stack
- System.IO for file operations
- Data structures such as lists and records for playlist management

## Project Structure

```
Stack_CLI_Haskell_Playlist_Generation_App/
├── app/
│   └── Main.hs
├── src/
│   ├── Playlist.hs
│   └── Track.hs
├── test/
│   └── Spec.hs
├── README.md
├── stack.yaml
└── package.yaml
```

## Contributing

Contributions are welcome! Please fork the repository and submit pull requests for any improvements or bug fixes.

1. Fork the repo
2. Create your feature branch (`git checkout -b feature/YourFeature`)
3. Commit your changes (`git commit -am 'Add some feature'`)
4. Push to the branch (`git push origin feature/YourFeature`)
5. Open a Pull Request

## License

This project is licensed under the MIT License.

---

*Coursework project for ECS713P - Functional Programming (2022/23).*
