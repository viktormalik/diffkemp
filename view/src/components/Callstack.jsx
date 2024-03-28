// Call stack visualisation.
// Author: Lukas Petr

import ListGroup from 'react-bootstrap/ListGroup';
import { PropTypes } from 'prop-types';
import { CallstackPropTypes, DefinitionsPropTypes } from '../PropTypesValues';

// In which call stack is the call located.
export const CallType = {
  Both: 'both',
  New: 'new',
  Old: 'old',
};

const SelectedFunPropType = PropTypes.shape({
  name: PropTypes.string,
  type: PropTypes.oneOf(Object.values(CallType)),
});

/**
 * Component for visualisation of call stack.
 * @param {Object} props
 * @param {string} props.compFunName - First (compared) function name.
 * @param {Object} props.selectedFunction - Function which is selected and should
 *   be highlighted in the call stack.
 * @param {String} props.selectedFunction.name - Name of the selected function.
 * @param {String} props.selectedFunction.type - CallType of the selected function.
 * @param {Function} props.onSelect - Callback function accepting a selected function,
 *   the callback is called when call (function) is clicked (selected).
 * @returns Returns call stack component.
 */
export default function Callstack({
  compFunName,
  oldCallStack,
  newCallStack,
  selectedFunction,
  onSelect,
  definitions,
}) {
  function getNewName(oldName) {
    return definitions?.[oldName]?.new?.name
      ? definitions[oldName].new.name
      : oldName;
  }
  /**
   * Returns tuple [old index, new index] where old index is index
   * to old callstack and new index is index to new callstack.
   * Calls on these indexes belongs together (by name) or because they
   * are last calls in callstacks.
   * If matching pair is not found returns null.
   * @param {Number} oldStart Start index to old callstack from which
   * should be found coresponding pair of calls.
   * @param {Number} newStart Start index to new callstack.
   */
  function findCorespondingPair(oldStart, newStart) {
    for (
      let oldIndex = oldStart;
      oldIndex < oldCallStack.length;
      oldIndex += 1
    ) {
      const newName = getNewName(oldCallStack[oldIndex].name);
      const newIndex = newCallStack.findIndex(
        (call, index) => (index >= newStart && call.name === newName),
      );
      if (newIndex !== -1) {
        return [oldIndex, newIndex];
      }
    }
    const lastOldIndex = oldCallStack.length - 1;
    const lastNewIndex = newCallStack.length - 1;
    if (oldStart <= lastOldIndex && newStart <= lastNewIndex) {
      return [lastOldIndex, lastNewIndex];
    }
    return null;
  }

  // Creating callstack
  const callstack = [];
  // adding compared function
  callstack.push(
    <SingleCall
      key={compFunName}
      oldName={compFunName}
      onSelect={onSelect}
      selectedFunction={selectedFunction}
    />,
  );
  // adding calls
  let newIndex = 0;
  let oldIndex = 0;
  while (oldIndex < oldCallStack.length) {
    const newName = getNewName(oldCallStack[oldIndex].name);
    // current calls correspond by name or are last calls
    if (
      newCallStack[newIndex].name === newName
            || (oldIndex === oldCallStack.length - 1
                && newIndex === newCallStack.length - 1)
    ) {
      callstack.push(
        <SingleCall
          key={oldCallStack[oldIndex].name}
          oldName={oldCallStack[oldIndex].name}
          newName={newCallStack[newIndex].name}
          onSelect={onSelect}
          selectedFunction={selectedFunction}
        />,
      );
      newIndex += 1;
      oldIndex += 1;
    } else {
      const match = findCorespondingPair(oldIndex, newIndex);
      if (match) {
        const [oldMatchIndex, newMatchIndex] = match;
        // inserting all calls up to the corresponding pair
        // from old and new callstack
        callstack.push(
          <Calls
            key={`${oldCallStack[oldIndex].name} + ${newCallStack[newIndex].name}`}
            oldCalls={oldCallStack.slice(oldIndex, oldMatchIndex)}
            newCalls={newCallStack.slice(newIndex, newMatchIndex)}
            onSelect={onSelect}
            selectedFunction={selectedFunction}
          />,
        );
        newIndex = newMatchIndex;
        oldIndex = oldMatchIndex;
      } else {
        console.error('Callstack error: not found coresponding pair');
        break;
      }
    }
  }

  return (
    <div className="callstack" data-testid="callstack">
      <div className="ms-1">
        <b>Call stack</b>
      </div>
      <ListGroup className="callstack-calls" as="ul">
        {callstack}
      </ListGroup>
    </div>
  );
}

Callstack.propTypes = {
  compFunName: PropTypes.string.isRequired,
  oldCallStack: CallstackPropTypes.isRequired,
  newCallStack: CallstackPropTypes.isRequired,
  selectedFunction: SelectedFunPropType,
  onSelect: PropTypes.func.isRequired,
  definitions: DefinitionsPropTypes.isRequired,
};

/**
 * Component for visualisation of pair of calls which belongs together.
 * @param {Object} props
 * @param {string} props.oldName - Name of old call (function).
 * @param {string} props.newName - Name of new call (function).
 * @param {Object} props.selectedFunction - Function which is selected and should
 *   be highlighted in the call stack.
 * @param {Function} props.onSelect - Callback function accepting a selected function.
 * @returns
 */
function SingleCall({
  oldName,
  newName = oldName,
  selectedFunction,
  onSelect,
}) {
  if (oldName === newName) {
    return (
      <Call
        name={oldName}
        onSelect={onSelect}
        selectedFunction={selectedFunction}
        type={CallType.Both}
      />
    );
  }
  return (
    <Calls
      type={CallType.Both}
      oldCalls={[{ name: oldName }]}
      newCalls={[{ name: newName }]}
      onSelect={onSelect}
      selectedFunction={selectedFunction}
    />
  );
}

SingleCall.propTypes = {
  oldName: PropTypes.string.isRequired,
  newName: PropTypes.string,
  selectedFunction: SelectedFunPropType,
  onSelect: PropTypes.func.isRequired,
};

/**
 * Component for visualisation of multiple calls from call stack
 * which does not belong together.
 * @param {Object} props
 * @param {Object[]} props.oldCalls - Calls from old call stack.
 * @param {Object[]} props.newCalls - Calls from new call stack.
 * @param {Object} props.selectedFunction - Function which is selected and should
 *   be highlighted in the call stack.
 * @param {Function} props.onSelect - Callback function accepting a selected function.
 * @returns
 */
function Calls({
  oldCalls, newCalls, selectedFunction, onSelect, type = null,
}) {
  return (
    <ListGroup horizontal as="ul" className="callstack-call-group">
      {/* old calls */}
      <ListGroup as="ul" className="callstack-subcalls">
        {oldCalls.map((call) => (
          <Call
            key={call.name}
            name={call.name}
            onSelect={onSelect}
            selectedFunction={selectedFunction}
            type={type || CallType.Old}
          />
        ))}
      </ListGroup>
      {/* new calls */}
      <ListGroup as="ul" className="callstack-subcalls">
        {newCalls.map((call) => (
          <Call
            key={call.name}
            name={call.name}
            onSelect={onSelect}
            selectedFunction={selectedFunction}
            type={type || CallType.New}
          />
        ))}
      </ListGroup>
    </ListGroup>
  );
}

Calls.propTypes = {
  type: PropTypes.string,
  oldCalls: CallstackPropTypes.isRequired,
  newCalls: CallstackPropTypes,
  selectedFunction: SelectedFunPropType,
  onSelect: PropTypes.func.isRequired,
};

/**
 * Component for visualisation of call from call stack.
 * @param {Object} props
 * @param {string} props.name - Name of called function/macro/type.
 * @param {Object} props.selectedFunction - Function which is selected and should
 *   be highlighted in the call stack.
 * @param {Function} props.onSelect - Callback function accepting a selected function.
 * @param {string} props.type - TypeCall of the call.
 * @returns
 */
function Call({
  name,
  selectedFunction,
  onSelect,
  type,
}) {
  // putting information about type/macro under the name of function
  const nameWrap = name.replace(' ', '<br/>');
  // Note: Currently checking if the function should be active only based on the name
  // (kind is omitted). This solves special cases of function-macro/macro-function differences
  // when we want to show their code at the same time.
  // If there will be other situations when we want to show code of functions with different
  // names simultaneously, then this needs to be reworked.
  return (
    <ListGroup.Item
      as="li"
      title={name}
      action
      className="callstack-call"
      onClick={() => onSelect({ name, type })}
      active={name.split(' ')[0] === selectedFunction?.name.split(' ')[0]
        && type === selectedFunction?.type}
      dangerouslySetInnerHTML={{ __html: nameWrap }}
    />
  );
}

Call.propTypes = {
  name: PropTypes.string.isRequired,
  selectedFunction: SelectedFunPropType,
  onSelect: PropTypes.func.isRequired,
  type: PropTypes.string.isRequired,
};
