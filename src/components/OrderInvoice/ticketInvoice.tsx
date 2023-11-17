import React, { useEffect, useState } from 'react';
import LogoImage from '~/public/assets/logo.png';
import NextImage from '../ui/img';
import { displayDate } from '~/utils/helper';

export default function TicketInvoice() {
  const [eventTickets, setEventTickets] = useState<any>(null);

  useEffect(() => {
    if (eventTickets) {
      const printContents = document?.getElementById('divToPrint')
        ?.innerHTML as string;
      const originalContents = document.body.innerHTML;
      document.body.innerHTML = printContents;
      const handleAfterPrint = () => {
        window.close();
      };
      window.addEventListener('afterprint', handleAfterPrint);
      window.print();
      document.body.innerHTML = originalContents;
    } else {
      const eventTicketPayload = JSON.parse(
        localStorage.getItem('event_tickets') as string,
      );
      setEventTickets(eventTicketPayload);
      localStorage.removeItem('event_tickets');
    }
  }, [eventTickets]);

  return (
    <div
      className="bg-card text-gray-400 rounded-lg mx-auto w-full min-h-screen px-8 py-10 sm:w-3/4 md:w-2/3"
      id="divToPrint"
    >
      <div className="flex flex-col md:flex-row items-center justify-between mb-8">
        <div className="flex items-center">
          <NextImage
            className="h-16  object-contain mr-2"
            src={LogoImage}
            alt="Logo"
          />
        </div>
        <div className="text-gray-400 xs:text-center sm:text-left">
          <div className="font-bold text-xl mb-2">INVOICE</div>
          <div className="text-sm">
            Date: {displayDate(eventTickets?.createdAt)}
          </div>
          <div className="text-sm">Invoice: #INV00{eventTickets?.orderId}</div>
        </div>
      </div>
      <h3 className="text-2xl text-center font-bold">Ticket Number List</h3>

      <div className="space-y-4">
        <h2 className="text-2xl font-bold">{eventTickets?.eventName}</h2>
        <div className="grid grid-cols-4 gap-2 md:grid-cols-6">
          {eventTickets?.tickets?.map((ticket: number) => (
            <p className={`w-20`} key={ticket}>
              #{ticket}
            </p>
          ))}
        </div>
      </div>
    </div>
  );
}
