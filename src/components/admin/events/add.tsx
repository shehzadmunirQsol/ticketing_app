import EventForm from "~/components/common/forms/events";
import GlobalBack from "~/components/common/globalBack";

export default function AddEvents() {
  return (
    <div className=" p-8 space-y-8 ">
      <div className="flex items-center justify-between">
        <h2 className="text-4xl font-medium">Add Event</h2>
        <GlobalBack />
      </div>
      <EventForm />
    </div>
  );
}
