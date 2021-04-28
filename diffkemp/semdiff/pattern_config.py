"""Configuration for difference code patterns."""
from subprocess import check_call, CalledProcessError, DEVNULL
import os
import yaml


class UnsupportedPatternError(ValueError):
    """Indicates that a pattern file uses an unsupported format."""
    pass


class PatternConfig:
    """
    Configuration file containing difference code patterns and global pattern
    comparison settings.
    Difference patterns can be used during module comparison to prevent
    the reporting of known and desired semantic differences.
    """
    def __init__(self, path=None, patterns_path=None):
        """
        Create a new difference pattern configuration.
        :param path: Path to the configuration source.
        """
        self.path = path
        self.patterns_path = patterns_path
        self.pattern_files = set()

        self.settings = {
            "on_parse_failure": "ERROR"
        }

        self.llvm_only = True

    @classmethod
    def create_from_file(cls, path, patterns_path=None):
        """
        Creates a new difference pattern configuration from a single pattern
        file or a configuration YAML.
        :param path: Path to the file to load.
        """
        PatternConfig.raise_for_invalid_file(path)
        config = cls(path, patterns_path)

        # Process the configuration file based on its extension.
        if PatternConfig.get_extension(path) not in ("ll"):
            config._load_yaml()
        else:
            config.add_pattern(path)

        return config

    @staticmethod
    def raise_for_invalid_file(path):
        """
        Ensures that the given file is readable. Raises an exception if not.
        :param path: Path to the file to check.
        """
        if not os.access(path, os.R_OK):
            raise ValueError(f"unable to read file {path}")

    @staticmethod
    def get_extension(path):
        """
        Retrives the extension from the given file.
        :param path: Path to the file.
        :return: The file extension, excluding the leading dot.
        """
        _, ext = os.path.splitext(path)
        return ext.lower()[1:]

    def add_pattern(self, path):
        """
        Tries to add a new pattern file, converting it to LLVM IR if necessary.
        :param path: Path to the pattern file to add.
        """
        if self.patterns_path:
            full_path = os.path.join(self.patterns_path, path)
        else:
            full_path = path

        PatternConfig.raise_for_invalid_file(full_path)
        filetype = PatternConfig.get_extension(full_path)

        # Convert all supported pattern types to LLVM IR.
        # Currently, only LLVM IR patterns are supported.
        if filetype == "ll":
            self._add_llvm_pattern(full_path)
        else:
            # Inform about unexpected file extensions.
            self._on_parse_failure(f"{full_path}: {filetype} pattern files "
                                   f"are not supported")

    def _load_yaml(self):
        """Load the pattern configuration from its YAML representation."""
        with open(self.path, "r") as config_yaml:
            yaml_dict = yaml.safe_load(config_yaml)

        if "patterns" not in yaml_dict:
            return

        # Process global pattern settings.
        if "on_parse_failure" in yaml_dict:
            self.settings["on_parse_failure"] = yaml_dict["on_parse_failure"]

        for pattern in yaml_dict["patterns"]:
            self.add_pattern(pattern)

    def _add_llvm_pattern(self, path):
        """
        Tries to add a new LLVM IR pattern file.
        :param path: Path to the LLVM IR pattern file to add.
        """
        try:
            # Ensure that the pattern is valid.
            check_call(["opt", "-verify", path], stdout=DEVNULL)
        except CalledProcessError:
            self._on_parse_failure(f"failed to parse pattern file {path}")

        self.pattern_files.add(path)

    def _on_parse_failure(self, message):
        """
        Inform user about a pattern parse failure.
        :param message: Message for the user.
        """
        if self.settings["on_parse_failure"] == "ERROR":
            raise UnsupportedPatternError(message)
        elif self.settings["on_parse_failure"] == "WARN":
            print(message)
