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
    superagent.post('/api/entries?key=' + this.state.key, entry, function(response) {
      this.setState({entries: [this.transform(response.body)].concat(this.state.entries)})
    }.bind(this));
  },
  handleEntryDelete: function (number) {
    superagent.del('/api/entries/' + number + '?key=' + this.state.key, function () {
      this.getEntries();
    }.bind(this));
  },
  handleEntryUpdate: function(entry) {
    superagent.put('/api/entries/' + entry.number + '?key=' + this.state.key, entry, function() {
      this.getEntries();
    }.bind(this));
  },
  getInitialState: function() {
    return {entries: [], key: window.location.hash.substring(1)}
  },
  componentDidMount: function() {
    this.getEntries();
  },
  getEntries: function() {
    superagent.get('/api/entries?key=' + this.state.key, function(response) {
      this.setState({entries: response.body.map(this.transform)});
    }.bind(this));
  },
  render: function() {
    return (
      <div>
        <EntryBalance entries={this.state.entries} />
        <EntryForm onEntrySubmit={this.handleEntrySubmit} />
        <EntryList
          entries={this.state.entries}
          onEntryDelete={this.handleEntryDelete}
          onEntryUpdate={this.handleEntryUpdate}
        />
      </div>
    );
  }
});

var EntryBalance = React.createClass({
  getBalance: function() {
    return this.props.entries
      .map(function(e) { return e.amount; })
      .reduce(function(x, y) { return x + y; }, 0);
  },
  getClass: function() {
    var balance = this.getBalance();

    if (balance > 0) { return 'positive'; }
    if (balance < 0) { return 'negative'; }
  },
  render: function() {
    return (
      <div className="balance">
        <div className="balance-label">
          Balance
        </div>

        <div className={['balance-value', this.getClass()].join(' ')}>
          ${this.getBalance().toFixed(2)}
        </div>
      </div>
    );
  }
});

var EntryForm = React.createClass({
  handleSubmit: function(event) {
    var amountNode = this.refs.amount.getDOMNode();
    var amount = amountNode.valueAsNumber;
    var nameNode = this.refs.name.getDOMNode();
    var name = nameNode.value;
    var entry = {amount: amount, name: name, number: 0};

    event.preventDefault();

    if (isNaN(amount) || !isFinite(amount)) {
      amountNode.focus();
      return;
    }

    if (!name) {
      nameNode.focus();
      return;
    }

    amountNode.value = '';
    amountNode.blur();
    nameNode.value = '';
    nameNode.blur();

    this.props.onEntrySubmit(entry);
  },
  render: function() {
    return (
      <fieldset className="form">
        <legend className="form-legend">Create an entry</legend>

        <form onSubmit={this.handleSubmit}>
          <ol className="form-list">
            <li className="form-element">
              <label className="form-label" htmlFor="amount">Amount</label>
              <input className="form-input" type="number" ref="amount" placeholder="7.31" id="amount" />
            </li>

            <li>
              <label className="form-label" htmlFor="name">Name</label>
              <input className="form-input" ref="name" placeholder="Lunch at Freebirds" id="name" />
            </li>

            <li>
              <input className="form-submit" type="submit" value="Create" />
            </li>
          </ol>
        </form>
      </fieldset>
    );
  }
});

var EntryList = React.createClass({
  handleEntryDelete: function(number) {
    this.props.onEntryDelete(number);
  },
  handleEntryUpdate: function(entry) {
    this.props.onEntryUpdate(entry);
  },
  renderEntryElements: function() {
    return this.props.entries.sort(function (a, b) {
      return b.date - a.date;
    }).map(function (entry) {
      return (
        <EntryElement
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
  },
  render: function() {
    return (
      <ol className="entries">
        {this.renderEntryElements()}
      </ol>
    );
  }
});

var EntryElement = React.createClass({
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
      <li className="entry">
        <div className="entry-amount">
          ${this.props.amount.toFixed(2)}
        </div>

        <div className="entry-name">
          {this.props.name}
        </div>

        <div className="entry-date">
          <time dateTime={this.props.date.toISOString()}>
            {this.props.date.toLocaleDateString()}
          </time>
        </div>

        <button className="entry-edit" onClick={this.handleEditClick}>Edit</button>
        <button className="entry-delete" onClick={this.handleDeleteClick}>Delete</button>
      </li>
    );
  },
  renderEdit: function() {
    return (
      <li className="entry">
        <form onSubmit={this.handleSaveClick}>
          <div className="entry-amount">
            <input type="number" ref="amount" defaultValue={this.props.amount} />
          </div>

          <div className="entry-name">
            <input ref="name" defaultValue={this.props.name} />
          </div>

          <div className="entry-date">
            <time dateTime={this.props.date.toISOString()}>
              {this.props.date.toLocaleDateString()}
            </time>
          </div>

          <input type="submit" value="Save" className="entry-save" />
          <button className="entry-cancel" onClick={this.handleCancelClick}>Cancel</button>
        </form>
      </li>
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
