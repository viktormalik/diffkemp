import { PropTypes } from 'prop-types';

export const CallStackCallPropTypes = PropTypes.shape({
  name: PropTypes.string,
  file: PropTypes.string,
  line: PropTypes.number,
});

export const CallstackPropTypes = PropTypes.arrayOf(CallStackCallPropTypes);

export const DiffPropTypes = PropTypes.shape({
  function: PropTypes.string,
  'old-callstack': CallstackPropTypes,
  'new-callstack': CallstackPropTypes,
});

const DefinitionShape = {
  kind: PropTypes.oneOf(['function', 'type', 'macro', 'macro-function',
    'function-macro', 'assembly']),
  old: PropTypes.shape({
    line: PropTypes.number,
    file: PropTypes.string,
    'end-line': PropTypes.number,
  }),
  new: PropTypes.shape({
    line: PropTypes.number,
    file: PropTypes.string,
    name: PropTypes.string,
    'end-line': PropTypes.number,
  }),
  diff: PropTypes.bool,
};

export const DefinitionsPropTypes = PropTypes.objectOf(PropTypes.shape(DefinitionShape));
