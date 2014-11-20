var Ledger = React.createClass({
  render: function() {
    return (
      <div>
        <Navigation />

        <div className="container-fluid">
          <div className="row">
            <div className="col-sm-4">
              <NewEntry />
            </div>

            <div className="col-sm-8">
              <Entries />
            </div>
          </div>
        </div>
      </div>
    );
  }
});

var Navigation = React.createClass({
  render: function() {
    return (
      <nav className="navbar navbar-default navbar-static-top">
        <div className="container-fluid">
          <div className="navbar-header">
            <a className="navbar-brand" href="/">
              Ledger
            </a>
          </div>
        </div>
      </nav>
    );
  }
});

var NewEntry = React.createClass({
  render: function() {
    return (
      <div className="panel panel-default">
        <div className="panel-heading">
          Create an entry
        </div>

        <div className="panel-body">
          <EntryForm />
        </div>
      </div>
    );
  }
});

var EntryForm = React.createClass({
  render: function() {
    return (
      <form>
        <div className="radio">
          <label>
            <input type="radio" name="type" defaultChecked />
            Credit
          </label>
        </div>

        <div className="radio">
          <label>
            <input type="radio" name="type" />
            Debit
          </label>
        </div>

        <div className="form-group">
          <label htmlFor="amount">
            Amount
          </label>
          <div className="input-group">
            <div className="input-group-addon">
              $
            </div>
            <input type="number" className="form-control" id="amount" placeholder="7.31" />
          </div>
        </div>

        <div className="form-group">
          <label htmlFor="name">
            Name
          </label>
          <input type="text" className="form-control" id="name" placeholder="Lunch at Freebirds" />
        </div>

        <button type="submit" className="btn btn-primary">
          Create
        </button>
      </form>
    );
  }
});

var Entries = React.createClass({
  render: function() {
    return (
      <div className="list-group">
        <Entry />
      </div>
    );
  }
});

var Entry = React.createClass({
  render: function() {
    return (
      <div className="list-group-item">
        <div className="lead">
          <div className="pull-right">
            <div className="label label-danger">
              $-7.31
            </div>
          </div>

          Lunch at Freebirds
        </div>

        November 19, 2014
      </div>
    );
  }
});

React.render(
  <Ledger />,
  document.getElementById('react')
);
