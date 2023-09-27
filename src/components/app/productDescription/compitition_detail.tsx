import React from 'react';

const CompititionDetail = ({ data }: any) => {
  const dataCode: any = data?.EventDescription[0].comp_details;
  return (
    <section id="CompititionDetail" className="space-x-4">
      <div>
        <div className="w-30 my-10  space-x-4 ">
          <p className="lg:text-5xl md:text-4xl text-2xl   font-black uppercase  indent-4">
            Competition Details
          </p>
          <div className="border-b-4 w-16 border-primary mt-4 mb-8"></div>

          {false ? (
            <div className="my-10">
              <p className="mb-20">{dataCode}</p>
            </div>
          ) : (
            <>
              <p className="lg:text-3xl text-1xl font-black ">
                Mercedes E63s AMG Night Edition
              </p>
              <div>
                <p>
                  The last of its kind. The Ferrari E63s AMG is one of the last
                  V8 powerhouses. With future cars becoming hybridized, this
                  4.0L twin-turbo V8 is truly something special. Producing 603
                  horsepower when standard, the E63 is one of the quickest
                  saloon cars on the road. Even more so with this car, as it has
                  been re-mapped to produce just over 800BHP!
                  <br />
                  <br />
                  Being a &#39;Night Edition,&#39; the car comes with a limited
                  slip differential, the AMG Sports Exhaust, and many more
                  optional extras to make this an ultimate performance saloon.
                  <br />
                  <br />
                  We are giving you the chance to win one of the best E63s out
                  there for only AED 1.29!
                  <br />
                  <br />
                  <br />
                  Only 13k Miles
                  <br />
                  Full Service History
                  <br />
                  4.0L V8 Twin Turbo Engine
                  <br />
                  2021 Model - Auto Gearbox
                  <br />
                  Satin Black Wrap
                </p>

                <br />
                <br />
                <p className="text-3xl">Modification</p>
                <br />
                <p>
                  DMC Re-map to 800BHP
                  <br />
                  Full Satin Black Wrap
                  <br />
                  De-Chrome Package
                  <br />
                  Custom Fabricated Downpipes
                  <br />
                  Vossen HF2 21inch Alloy Wheels
                  <br />
                  Lowering Links
                </p>
                <br />
                <br />
                <p className="text-3xl">Specifications</p>

                <p>
                  <br />
                  Half Leather Half Alcantara Front &amp; Rear Seats
                  <br />
                  4-Way Adjustable Heated Front Seats
                  <br />
                  AMG Performance Gages
                  <br />
                  AMG Valved Sports Exhausts
                  <br />
                  AMG Dynamic Suspension
                  <br />
                  AMG Electronic Rear Differential
                  <br />
                  AMG Drivers Package
                  <br />
                  Night Edition Package
                  <br />
                  Full Electonically Openable Panoramic Roof
                  <br />
                  Virtual Cockpit Display
                  <br />
                  Carbon Fibre Interior Pack
                  <br />
                  Burmester Surround Sound System
                  <br />
                  Fully Configurable Driving Modes
                  <br />
                  Lane Departure Warning System
                  <br />
                  Blind Spot Detection Warning
                  <br />
                  4-Matic All Wheel Drive System
                  <br />
                  Mercedes-Benz UX Multimedia System
                  <br />
                  360 Degree Camera
                  <br />
                  Galvanised Shift Paddles
                  <br />
                  Rear Privacy Glass
                  <br />
                  Silver AMG Seatbelts
                  <br />
                  Automatic Adaptive LED Headlights
                  <br />
                  Automatic Dimming Interior and Exterior Mirrors
                  <br />
                  Android Auto &amp; Apple Carplay
                </p>
              </div>
            </>
          )}
        </div>
      </div>
    </section>
  );
};

export default CompititionDetail;
