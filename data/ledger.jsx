var EntryBox = React.createClass({
  transform: function(object) {
    return {
      amount: object.amount,
      date: new Date(object.created),
      key: object.number,
      name: object.name,
      number: object.number
    };
  },
  handleEntrySubmit: function(entry) {
    superagent.post('/api/entries', entry, function(response) {
      this.setState({entries: [this.transform(response.body)].concat(this.state.entries)})
    }.bind(this));
  },
  handleEntryClick: function (number) {
    superagent.del('/api/entries/' + number, function () {
      this.getEntries();
    }.bind(this));
  },
  getInitialState: function() {
    return {entries: []}
  },
  componentDidMount: function() {
    this.getEntries();
  },
  getEntries: function() {
    superagent.get('/api/entries', function(response) {
      this.setState({entries: response.body.map(this.transform)});
    }.bind(this));
  },
  render: function() {
    return (
      <div>
        <h2>Entries</h2>
        <EntryForm onEntrySubmit={this.handleEntrySubmit} />
        <EntryTable
          entries={this.state.entries}
          onEntryClick={this.handleEntryClick}
        />
      </div>
    );
  }
});

var EntryForm = React.createClass({
  handleSubmit: function(e) {
    var amount = this.refs.amount.getDOMNode().valueAsNumber;
    var name = this.refs.name.getDOMNode().value;

    if (!isNaN(amount) && isFinite(amount) && name) {
      var entry = {amount: amount, name: name, number: 0};
      this.props.onEntrySubmit(entry);
      this.refs.amount.getDOMNode().value = '';
      this.refs.name.getDOMNode().value = '';
    }

    e.preventDefault();
  },
  render: function() {
    return (
      <div>
        <form onSubmit={this.handleSubmit}>
          <input type="number" ref="amount" />
          <input ref="name" />
          <input type="submit" />
        </form>
      </div>
    );
  }
});

var EntryTable = React.createClass({
  handleEntryClick: function(number) {
    this.props.onEntryClick(number);
  },
  render: function() {
    var entryRows = this.props.entries.sort(function (a, b) {
      return b.date - a.date;
    }).map(function (entry) {
      return (
        <EntryRow
          amount={entry.amount}
          date={entry.date}
          key={entry.key}
          name={entry.name}
          number={entry.number}
          onEntryClick={this.handleEntryClick}
        />
      );
    }.bind(this));
    return (
      <table>
        <thead>
          <tr>
            <th>ID</th>
            <th>Amount</th>
            <th>Date</th>
            <th>Name</th>
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
  handleClick: function(e) {
    if (window.confirm('Are you sure you want to delete this entry?')) {
      this.props.onEntryClick(this.props.number);
    }
    e.preventDefault();
  },
  render: function() {
    return (
      <tr>
        <td>{this.props.number}</td>
        <td>${this.props.amount.toFixed(2)}</td>
        <td>
          <time dateTime="{this.props.date.toISOString()}">
            {this.props.date.toLocaleString()}
          </time>
        </td>
        <td>{this.props.name}</td>
        <td>
          <button onClick={this.handleClick}>Delete</button>
        </td>
      </tr>
    );
  }
});

React.render(
  <EntryBox />,
  document.getElementById('content')
);
