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
  handleEntryDelete: function (number) {
    superagent.del('/api/entries/' + number, function () {
      this.getEntries();
    }.bind(this));
  },
  handleEntryUpdate: function(entry) {
    superagent.put('/api/entries/' + entry.number, entry, function() {
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
          onEntryDelete={this.handleEntryDelete}
          onEntryUpdate={this.handleEntryUpdate}
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
  handleEntryDelete: function(number) {
    this.props.onEntryDelete(number);
  },
  handleEntryUpdate: function(entry) {
    this.props.onEntryUpdate(entry);
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
          onEntryDelete={this.handleEntryDelete}
          onEntryUpdate={this.handleEntryUpdate}
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
  getInitialState: function() {
    return {editing: false};
  },
  handleEditClick: function(e) {
    this.setState({editing: true});
    e.preventDefault();
  },
  handleDeleteClick: function(e) {
    if (window.confirm('Are you sure you want to delete this entry?')) {
      this.props.onEntryDelete(this.props.number);
    }
    e.preventDefault();
  },
  handleSaveClick: function(e) {
    var amount = this.refs.amount.getDOMNode().valueAsNumber;
    var name = this.refs.name.getDOMNode().value;

    if (!isNaN(amount) && isFinite(amount) && name) {
      var entry = {amount: amount, name: name, number: this.props.number};
      this.props.onEntryUpdate(entry);
      this.setState({editing: false});
    }

    e.preventDefault();
  },
  handleCancelClick: function(e) {
    this.setState({editing: false});
    e.preventDefault();
  },
  renderShow: function() {
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
          <button onClick={this.handleEditClick}>Edit</button>
          <button onClick={this.handleDeleteClick}>Delete</button>
        </td>
      </tr>
    );
  },
  renderEdit: function() {
    return (
      <tr>
        <td>{this.props.number}</td>
        <td>
          <input type="number" ref="amount" defaultValue={this.props.amount} />
        </td>
        <td>
          <time dateTime="{this.props.date.toISOString()}">
            {this.props.date.toLocaleString()}
          </time>
        </td>
        <td>
          <input ref="name" defaultValue={this.props.name} />
        </td>
        <td>
          <button onClick={this.handleSaveClick}>Save</button>
          <button onClick={this.handleCancelClick}>Cancel</button>
        </td>
      </tr>
    );
  },
  render: function() {
    if (this.state.editing) {
      return this.renderEdit();
    } else {
      return this.renderShow();
    }
  }
});

React.render(
  <EntryBox />,
  document.getElementById('content')
);
