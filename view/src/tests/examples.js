// Examples of differences (real or created) for testing functionality of the viewer

/* eslint import/prefer-default-export: "off" */

export const MacroFunctionDifference = {
  results: [{
    // compared function
    function: 'down_write',
    diffs: [{
      // differing function
      function: 'get_task_struct',
      'old-callstack': [{ name: 'get_task_struct (macro)' }],
      'new-callstack': [{ name: 'get_task_struct' }],
    }],
  }],
  // Expected call stack visualisation:
  // ----------------------------------------------------------------
  // | down_write                                                   |
  // |--------------------------------------------------------------|
  // | get_task_struct (macro)         | get_task_struct            |
  // ----------------------------------------------------------------
};
