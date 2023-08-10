import React from 'react';

function index() {
  return (
    <>
      <header>My Header</header>
      <div className="flex">
        Sidebar
        <div className="flex-1">Main content</div>
      </div>
    </>
  );
}

export default index;
