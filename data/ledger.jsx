var Ledger = React.createClass({
  render: function() {
    return (
      <div>
        <Header />
        <Content
          entries={this.state.entries}
          onCreateEntry={this.createEntry}
          onUpdateEntry={this.updateEntry}
          onDeleteEntry={this.deleteEntry}
          />
        <Footer />
      </div>
    );
  },
  getInitialState: function() {
    return {
      entries: [],
      key: window.location.hash.substring(1)
    };
  },
  componentWillMount: function() {
    this.getEntries();
  },

  getEntries: function() {
    var url = '/api/entries?key=' + this.state.key;
    superagent.get(url, function(response) {
      var entries = response.body.map(this.transform);
      this.setState({entries: entries});
    }.bind(this));
  },
  transform: function(object) {
    return {
      amount: object.amount,
      created: new Date(object.created),
      name: object.name,
      number: object.number
    };
  },
  createEntry: function(entry) {
    var url = '/api/entries?key=' + this.state.key;
    superagent.post(url, entry, function(response) {
      var newEntry = this.transform(response.body);
      this.setState({entries: [newEntry].concat(this.state.entries)});
    }.bind(this));
  },
  updateEntry: function(entry) {
    var url = '/api/entries/' + entry.number + '?key=' + this.state.key;
    superagent.put(url, entry, function(response) {
      var updatedEntry = this.transform(response.body);
      this.setState({entries: this.state.entries.map(function(e) {
        if (e.number === updatedEntry.number) {
          return updatedEntry;
        }
        else {
          return e;
        }
      })});
    }.bind(this));
  },
  deleteEntry: function(entry) {
    var url = '/api/entries/' + entry.number + '?key=' + this.state.key;
    superagent.del(url, function(response) {
      this.setState({entries: this.state.entries.filter(function(e) {
        return e.number !== entry.number;
      })});
    }.bind(this));
  }
});

var Header = React.createClass({
  render: function() {
    return (
      <div className="navbar navbar-default navbar-static-top">
        <div className="container">
          <div className="navbar-header">
            <a className="navbar-brand" href="/">
              Ledger
            </a>
          </div>
        </div>
      </div>
    );
  }
});

var Content = React.createClass({
  propTypes: {
    entries: React.PropTypes.array.isRequired,
    onCreateEntry: React.PropTypes.func.isRequired,
    onUpdateEntry: React.PropTypes.func.isRequired,
    onDeleteEntry: React.PropTypes.func.isRequired
  },

  render: function() {
    return (
      <div className="container">
        <div className="row">
          <div className="col-sm-4">
            <Balance entries={this.filteredEntries()} />
            <hr />
            <CreateEntryForm onCreate={this.props.onCreateEntry} />
          </div>

          <div className="col-sm-8">
            <Search onSearch={this.handleSearch} />
            <Filter onFilter={this.handleFilter} />
            <Entries
              entries={this.sortedEntries()}
              onUpdate={this.props.onUpdateEntry}
              onDelete={this.props.onDeleteEntry}
              />
          </div>
        </div>
      </div>
    );
  },
  getInitialState: function() {
    return {
      after: null,
      before: null,
      maximum: undefined,
      minimum: undefined,
      query: ''
    };
  },

  handleFilter: function(after, before, minimum, maximum) {
    this.setState({
      after: after,
      before: before,
      maximum: maximum,
      minimum: minimum
    });
  },
  handleSearch: function(query) {
    this.setState({query: query});
  },

  filteredEntries: function() {
    return this.props.entries.filter(function(entry) {
      if (this.state.query) {
        var haystack = entry.name.toLowerCase();
        var needle = this.state.query.toLowerCase();
        if (haystack.indexOf(needle) === -1) {
          return false;
        }
      }

      if (this.state.after) {
        if (entry.created < this.state.after) {
          return false;
        }
      }

      if (this.state.before) {
        if (entry.created > this.state.before) {
          return false;
        }
      }

      if (!isNaN(this.state.minimum)) {
        if (entry.amount < this.state.minimum) {
          return false;
        }
      }

      if (!isNaN(this.state.maximum)) {
        if (entry.amount > this.state.maximum) {
          return false;
        }
      }

      return true;
    }.bind(this));
  },
  sortedEntries: function() {
    return this.filteredEntries().sort(function(a, b) {
      return b.created - a.created;
    });
  }
});

var Footer = React.createClass({
  render: function() {
    return (
      <div className="container">
        <p className="text-center">
          <a href="https://github.com/tfausak/ledger">
            Ledger
          </a>
          {' '}
          v0.1.4
        </p>
      </div>
    );
  }
});

var Balance = React.createClass({
  propTypes: {
    entries: React.PropTypes.array.isRequired
  },

  render: function() {
    return (
      <h1 className="text-center">
        Balance
        {' '}
        <span className={['label', 'label-' + this.getClassName()].join(' ')}>
          ${this.getBalance().toFixed(2)}
        </span>
      </h1>
    );
  },

  getBalance: function() {
    return this.props.entries.map(function(entry) {
      return entry.amount;
    }).reduce(function(a, b) {
      return a + b
    }, 0);
  },
  getClassName: function() {
    if (this.isCredit()) {
      return 'danger';
    }
    if (this.isDebit()) {
      return 'success';
    }

    return 'default';
  },
  isCredit: function() {
    return this.getBalance() < 0;
  },
  isDebit: function() {
    return this.getBalance() > 0;
  }
});

var CreateEntryForm = React.createClass({
  propTypes: {
    onCreate: React.PropTypes.func.isRequired
  },

  render: function() {
    return (
      <div className="panel panel-default">
        <div className="panel-heading">
          Create an entry
        </div>

        <div className="panel-body">
          <EntryForm onCreate={this.props.onCreate} />
        </div>
      </div>
    );
  }
});

var Search = React.createClass({
  propTypes: {
    onSearch :React.PropTypes.func.isRequired
  },

  render: function() {
    return (
      <form>
        <div className="form-group">
          <input
            className="form-control"
            onChange={this.handleChange}
            placeholder="Search"
            ref="query"
            type="search"
            />
        </div>
      </form>
    );
  },

  handleChange: function() {
    this.props.onSearch(this.refs.query.getDOMNode().value);
  }
});

var Filter = React.createClass({
  propTypes: {
    onFilter: React.PropTypes.func.isRequired
  },

  render: function() {
    return (
      <form>
        <div className="row">
          <div className="form-group col-xs-6">
            <input
              className="form-control"
              onChange={this.handleChange}
              placeholder="After"
              ref="after"
              type="date"
              />
          </div>

          <div className="form-group col-xs-6">
            <input
              className="form-control"
              onChange={this.handleChange}
              placeholder="Before"
              ref="before"
              type="date"
              />
          </div>
        </div>

        <div className="row">
          <div className="form-group col-xs-6">
            <div className="input-group">
              <div className="input-group-addon">
                $
              </div>

              <input
                className="form-control"
                onChange={this.handleChange}
                placeholder="Minimum"
                ref="minimum"
                step="any"
                type="number"
                />
            </div>
          </div>

          <div className="form-group col-xs-6">
            <div className="input-group">
              <div className="input-group-addon">
                $
              </div>

              <input
                className="form-control"
                onChange={this.handleChange}
                placeholder="Maximum"
                ref="maximum"
                step="any"
                type="number"
                />
            </div>
          </div>
        </div>
      </form>
    );
  },

  handleChange: function() {
    var afterNode = this.refs.after.getDOMNode();
    var after = afterNode.valueAsDate ||
      new Date(Date.parse(afterNode.value));

    var beforeNode = this.refs.before.getDOMNode();
    var before = beforeNode.valueAsDate ||
      new Date(Date.parse(beforeNode.value));

    var minimum = this.refs.minimum.getDOMNode().valueAsNumber;
    var maximum = this.refs.maximum.getDOMNode().valueAsNumber;

    this.props.onFilter(after, before, minimum, maximum);
  }
});

var Entries = React.createClass({
  propTypes: {
    entries: React.PropTypes.array.isRequired,
    onUpdate: React.PropTypes.func.isRequired,
    onDelete: React.PropTypes.func.isRequired
  },

  render: function() {
    return (
      <div className="list-group">
        {this.renderEntries()}
      </div>
    );
  },
  renderEntries: function() {
    return this.props.entries.map(function(entry) {
      return (
        <div className="list-group-item" key={entry.number}>
          <Entry
            amount={entry.amount}
            created={entry.created}
            name={entry.name}
            number={entry.number}
            onUpdate={this.props.onUpdate}
            onDelete={this.props.onDelete}
            />
        </div>
      );
    }.bind(this));
  },
});

var Entry = React.createClass({
  propTypes: {
    amount: React.PropTypes.number.isRequired,
    created: React.PropTypes.instanceOf(Date).isRequired,
    name: React.PropTypes.string.isRequired,
    number: React.PropTypes.number.isRequired,
    onUpdate: React.PropTypes.func.isRequired,
    onDelete: React.PropTypes.func.isRequired
  },

  render: function() {
    if (this.state.isEditing) {
      return this.renderEdit();
    }
    else {
      return this.renderShow();
    }
  },
  renderEdit: function() {
    return (
      <EditEntryForm
        amount={this.props.amount}
        name={this.props.name}
        number={this.props.number}
        onUpdate={this.onUpdate}
        onCancel={this.onCancel}
        onDelete={this.onDelete}
        />
    );
  },
  renderShow: function() {
    return (
      <ShowEntry
        amount={this.props.amount}
        created={this.props.created}
        name={this.props.name}
        onClick={this.onClick}
        />
    );
  },
  getInitialState: function() {
    return {isEditing: false};
  },

  onClick: function(event) {
    event.preventDefault();
    this.setState({isEditing: true});
  },
  onUpdate: function(entry) {
    this.setState({isEditing: false});
    this.props.onUpdate(entry);
  },
  onCancel: function() {
    this.setState({isEditing: false});
  },
  onDelete: function() {
    this.setState({isEditing: false});
    this.props.onDelete(this.props);
  }
});

var EditEntryForm = React.createClass({
  propTypes: {
    amount: React.PropTypes.number.isRequired,
    name: React.PropTypes.string.isRequired,
    number: React.PropTypes.number.isRequired,
    onUpdate: React.PropTypes.func.isRequired,
    onCancel: React.PropTypes.func.isRequired,
    onDelete: React.PropTypes.func.isRequired
  },

  render: function() {
    return (
      <EntryForm
        amount={this.props.amount}
        name={this.props.name}
        number={this.props.number}
        onUpdate={this.props.onUpdate}
        onCancel={this.props.onCancel}
        onDelete={this.props.onDelete}
        />
    );
  }
});

var ShowEntry = React.createClass({
  propTypes: {
    amount: React.PropTypes.number.isRequired,
    created: React.PropTypes.instanceOf(Date).isRequired,
    name: React.PropTypes.string.isRequired,
    onClick: React.PropTypes.func.isRequired,
  },

  render: function() {
    return (
      <div className="entry" onClick={this.props.onClick}>
        <div className="lead">
          <div className="pull-right">
            <div className={['label', 'label-' + this.getClassName()].join(' ')}>
              ${this.props.amount.toFixed(2)}
            </div>
          </div>

          {this.props.name}
        </div>

        <div>
          {this.props.created.toLocaleString()}
        </div>
      </div>
    );
  },

  getClassName: function() {
    if (this.isCredit()) {
      return 'danger';
    }
    if (this.isDebit()) {
      return 'success';
    }

    return 'default';
  },
  isCredit: function() {
    return this.props.amount < 0;
  },
  isDebit: function() {
    return this.props.amount > 0;
  }
});

var EntryForm = React.createClass({
  propTypes: {
    amount: React.PropTypes.number,
    name: React.PropTypes.string,
    number: React.PropTypes.number,
    onCreate: React.PropTypes.func,
    onUpdate: React.PropTypes.func,
    onCancel: React.PropTypes.func,
    onDelete: React.PropTypes.func
  },

  render: function() {
    return (
      <form id="entry-form" onSubmit={this.onSubmit}>
        <div className="form-group">
          <label>
            Type
          </label>

          <div className="radio">
            <label>
              <input
                defaultChecked={!this.props.amount || this.props.amount < 0}
                name="type"
                ref="type-credit"
                type="radio"
                value="credit"
                />
              Credit
            </label>
          </div>

          <div className="radio">
            <label>
              <input
                defaultChecked={this.props.amount > 0}
                name="type"
                ref="type-debit"
                type="radio"
                value="debit"
                />
              Debit
            </label>
          </div>
        </div>

        <div className="form-group">
          <label htmlFor="amount">
            Amount
          </label>

          <div className="input-group">
            <div className="input-group-addon">
              $
            </div>

            <input
              className="form-control"
              defaultValue={this.props.amount == undefined ? null : Math.abs(this.props.amount).toFixed(2)}
              id="amount"
              min="0"
              placeholder="7.31"
              ref="amount"
              step="any"
              type="number"
              />
          </div>
        </div>

        <div className="form-group">
          <label htmlFor="name">
            Name
          </label>

          <input
            className="form-control"
            defaultValue={this.props.name}
            id="name"
            placeholder="Lunch at Freebirds"
            ref="name"
            type="text"
            />
        </div>

        {this.renderButtons()}
      </form>
    );
  },
  renderButtons: function() {
    if (this.isEditing()) {
      return (
        <div className="btn-group">
          <button className="btn btn-primary" type="submit">
            Update
          </button>

          <button
            className="btn btn-default"
            onClick={this.onCancel}
            type="button"
            >
            Cancel
          </button>

          <button
            className="btn btn-danger"
            onClick={this.onDelete}
            type="button"
            >
            Delete
          </button>
        </div>
      );
    }
    else {
      return (
        <button className="btn btn-primary" type="submit">
          Create
        </button>
      );
    }
  },

  onSubmit: function(event) {
    event.preventDefault();

    var typeCreditNode = this.refs['type-credit'].getDOMNode();
    var typeDebitNode = this.refs['type-debit'].getDOMNode();
    var amountNode = this.refs.amount.getDOMNode();
    var amount = amountNode.valueAsNumber;
    var nameNode = this.refs.name.getDOMNode();
    var name = nameNode.value;

    if (isNaN(amount) || !isFinite(amount)) {
      amountNode.focus();
      return;
    }

    if (!name) {
      nameNode.focus();
      return;
    }

    if (typeCreditNode.checked) {
      amount *= -1;
    }

    var entry = {amount: amount, name: name};

    if (this.isEditing()) {
      entry = {
        amount: entry.amount,
        name: entry.name,
        number: this.props.number
      };
      this.props.onUpdate(entry);
    }
    else {
      this.props.onCreate(entry);

      typeCreditNode.checked = true;
      typeDebitNode.checked = false;

      var nodes = document.querySelectorAll('#entry-form input, #entry-form button');
      for (var i = 0; i < nodes.length; i++) {
        nodes.item(i).blur();
      }

      amountNode.value = '';
      nameNode.value = '';
    }
  },
  onCancel: function(event) {
    event.preventDefault();
    this.props.onCancel();
  },
  onDelete: function(event) {
    event.preventDefault();
    if (window.confirm('Are you sure you want to delete this entry?')) {
      this.props.onDelete();
    }
  },

  isEditing: function() {
    return this.props.onUpdate;
  }
});

React.render(
  <Ledger />,
  document.getElementById('ledger')
);
