"""
Kernel snapshot representation.
Contains mappings of original kernel source, snapshot kernel source and
holds the function list.
"""
from diffkemp.llvm_ir.kernel_module import LlvmKernelModule
from diffkemp.llvm_ir.kernel_source import KernelSource
import datetime
import os
import pkg_resources
import shutil
import yaml


class Snapshot:
    """
    Kernel snapshot.
    Provides methods for snapshot management. Also includes descriptions of
    functions and sysctl options.
    """

    class FunctionDesc:
        """
        Function description.
        Contains LLVM module of the function definition, a global variable,
        and a tag.
        For sysctl option, glob_var is the data variable that the option sets.
        """

        def __init__(self, mod, glob_var, indices, tag):
            self.mod = mod
            self.glob_var = glob_var
            self.var_indices = indices
            self.tag = tag

    class FunctionGroup:
        """
        Group of functions. Used to group functions belonging to a single
        sysctl options.
        """

        def __init__(self):
            self.functions = dict()

    def __init__(self, kernel_source=None, snapshot_source=None,
                 fun_kind=None):
        self.kernel_source = kernel_source
        self.snapshot_source = snapshot_source
        self.fun_kind = fun_kind
        self.fun_groups = dict()
        self.created_time = None

    @classmethod
    def create_from_source(cls, kernel_dir, output_dir, fun_kind=None,
                           setup_dir=True):
        """
        Create a snapshot from a kernel source directory and prepare it for
        snapshot directory generation.
        :param kernel_dir: Source kernel directory.
        :param output_dir: Snapshot output directory.
        :param fun_kind: Snapshot function kind.
        :param setup_dir: Whether to recreate the output directory.
        :return: Desired instance of Snapshot.
        """
        output_path = os.path.abspath(output_dir)

        # Cleanup or create the output directory of the new snapshot.
        if setup_dir:
            if os.path.isdir(output_path):
                shutil.rmtree(output_path)
            os.mkdir(output_path)

        # Prepare source representations for the new snapshot
        kernel_source = KernelSource(kernel_dir, True)
        snapshot_source = KernelSource(output_path)

        kernel_snapshot = cls(kernel_source, snapshot_source, fun_kind)

        # Add a new None group to the snapshot if the function kind is None.
        if fun_kind is None:
            kernel_snapshot._add_none_group()

        return kernel_snapshot

    @classmethod
    def load_from_dir(cls, snapshot_dir, config_file="snapshot.yaml"):
        """
        Loads a snapshot from its directory.
        :param snapshot_dir: Target snapshot directory.
        :param config_file: Name of the snapshot configuration file.
        :return: Desired instance of Snapshot.
        """
        snapshot_source = KernelSource(snapshot_dir)
        loaded_snapshot = cls(None, snapshot_source)

        with open(os.path.join(snapshot_dir, config_file), "r") as \
                snapshot_yaml:
            loaded_snapshot._from_yaml(snapshot_yaml.read())

        return loaded_snapshot

    def finalize(self):
        """Finalize both kernel sources."""
        if self.kernel_source is not None:
            self.kernel_source.finalize()
        if self.snapshot_source is not None:
            self.snapshot_source.finalize()

    def generate_snapshot_dir(self):
        """Generate the corresponding snapshot directory."""
        # Copy LLVM files to the snapshot.
        self.kernel_source.copy_source_files(self.modules(),
                                             self.snapshot_source.kernel_dir)
        self.snapshot_source.build_cscope_database()

        # Create the YAML snapshot representation inside the output directory.
        with open(os.path.join(self.snapshot_source.kernel_dir,
                               "snapshot.yaml"), "w") as snapshot_yaml:
            snapshot_yaml.write(self.to_yaml())

    def add_fun(self, name, llvm_mod, glob_var=None,
                var_indices=None, tag=None, group=None):
        """
        Add function to the function list.
        :param name: Name of the function.
        :param llvm_mod: LLVM module with the function definition.
        :param tag: Function tag.
        :param group: Group to put the function to.
        """
        self.fun_groups[group].functions[name] = self.FunctionDesc(llvm_mod,
                                                                   glob_var,
                                                                   var_indices,
                                                                   tag)

    def _add_none_group(self):
        """
        Add new group with None name which is used when only a single group
        is used.
        """
        self.fun_groups[None] = self.FunctionGroup()

    def add_fun_group(self, name):
        """
        Add new function group with the given name.
        :param name: Name of the group.
        """
        self.fun_groups[name] = self.FunctionGroup()

    def modules(self):
        """
        Get the set of all LLVM modules of all functions in the function list.
        """
        return set([fun.mod for group in self.fun_groups.values()
                    for fun in group.functions.values()
                    if fun.mod is not None])

    def get_by_name(self, name, group=None):
        """
        Get module for the function with the given name in the given group.
        :param name: Name of the function.
        :param group: Group of the function.
        """
        return self.fun_groups[group].functions[name] \
            if group in self.fun_groups and name in self.fun_groups[
            group].functions \
            else None

    def filter(self, functions, group=None):
        """
        Filter only chosen functions in the given group.
        :param name: List of function names to filter.
        :param group: Group to filter.
        """
        self.fun_groups[group].functions = {f: self.fun_groups[
            group].functions[f] for f in functions}

    def _from_yaml(self, yaml_file):
        """
        Load the snaphot from its YAML representation. Paths are assumed to be
        relative to the root directory.
        :param yaml_file: Contents of the YAML file.
        """
        yaml_loader = (yaml.CSafeLoader if "CSafeLoader" in yaml.__dict__
                       else yaml.SafeLoader)
        yaml_file = yaml.load(yaml_file, Loader=yaml_loader)
        yaml_dict = yaml_file[0]

        self.created_time = yaml_dict["created_time"]
        self.created_time = self.created_time.replace(
            tzinfo=datetime.timezone.utc)

        if os.path.isdir(yaml_dict["source_kernel_dir"]):
            self.kernel_source = KernelSource(yaml_dict["source_kernel_dir"],
                                              True)

        if "sysctl" in yaml_dict["list"][0]:
            self.kind = "sysctl"
            groups = yaml_dict["list"]
        else:
            groups = [yaml_dict["list"]]
        for g in groups:
            if "sysctl" in g:
                self.kind = "sysctl"
                group = g["sysctl"]
                functions = g["functions"]
            else:
                group = None
                functions = g
            self.fun_groups[group] = self.FunctionGroup()
            for f in functions:
                self.add_fun(f["name"],
                             LlvmKernelModule(
                                 os.path.join(os.path.relpath(
                                     self.snapshot_source.kernel_dir),
                                     f["llvm"]))
                             if f["llvm"] else None,
                             f["glob_var"], f["glob_var_indices"],
                             f["tag"],
                             group)

    def to_yaml(self):
        """
        Dump the snapshot as a YAML string. Paths to files are given relative
        to the source snapshot directory.
        :return: YAML string.
        """
        # Create the function group list.
        fun_yaml_dict = [{
            self.fun_kind: group_name,
            "functions": [{
                "name": fun_name,
                "llvm": os.path.relpath(fun_desc.mod.llvm,
                                        self.snapshot_source.kernel_dir)
                if fun_desc.mod else None,
                "glob_var": fun_desc.glob_var,
                "glob_var_indices": fun_desc.var_indices,
                "tag": fun_desc.tag
            } for fun_name, fun_desc in g.functions.items()]
        } for group_name, g in self.fun_groups.items()
        ]

        # Create the top level YAML structure.
        yaml_dict = [{
            "diffkemp_version": pkg_resources.require("diffkemp")[0].version,
            "created_time": datetime.datetime.now(datetime.timezone.utc),
            "kind": "function_list" if self.fun_kind is None else
            "systcl_group_list",
            "list": fun_yaml_dict[0]["functions"] if None in self.fun_groups
            else fun_yaml_dict,
            "source_kernel_dir": self.kernel_source.kernel_dir
        }]
        return yaml.dump(yaml_dict)
