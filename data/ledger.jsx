/** @jsx React.DOM */

var EntryBox = React.createClass({
  transform: function(object) {
    return {
      amount: object.amount,
      date: new Date(object.created),
      key: object.number
    };
  },
  handleEntrySubmit: function(entry) {
    superagent.post('/entries', entry, function(response) {
      this.setState({entries: [this.transform(response.body)].concat(this.state.entries)})
    }.bind(this));
  },
  getInitialState: function() {
    return {entries: []}
  },
  componentDidMount: function() {
    superagent.get('/entries', function(response) {
      this.setState({entries: response.body.map(this.transform)});
    }.bind(this));
  },
  render: function() {
    return (
      <div>
        <h2>Entries</h2>
        <EntryForm onEntrySubmit={this.handleEntrySubmit} />
        <EntryTable entries={this.state.entries}/>
      </div>
    );
  }
});

var EntryForm = React.createClass({
  handleSubmit: function(e) {
    var amount = this.refs.amount.getDOMNode().valueAsNumber;

    if (!isNaN(amount) && isFinite(amount)) {
      var entry = {amount: amount, number: 0};
      this.props.onEntrySubmit(entry);
      this.refs.amount.getDOMNode().value = '';
    }

    e.preventDefault();
    return;
  },
  render: function() {
    return (
      <div>
        <form onSubmit={this.handleSubmit}>
          <input type="number" ref="amount" />
          <input type="submit" />
        </form>
      </div>
    );
  }
});

var EntryTable = React.createClass({
  render: function() {
    var entryRows = this.props.entries.map(function (entry) {
      return (
        <EntryRow amount={entry.amount} date={entry.date} key={entry.key} />
      );
    });
    return (
      <table>
        <thead>
          <tr>
            <th>ID</th>
            <th>Amount</th>
            <th>Date</th>
          </tr>
        </thead>
        <tbody>
          {entryRows}
        </tbody>
      </table>
    );
  }
});

var EntryRow = React.createClass({
  render: function() {
    return (
      <tr>
        <td>{this.props.key}</td>
        <td>${this.props.amount.toFixed(2)}</td>
        <td>{this.props.date.toISOString()}</td>
      </tr>
    );
  }
});

React.renderComponent(
  <EntryBox />,
  document.getElementById('content')
);
