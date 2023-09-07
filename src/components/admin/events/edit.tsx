import EventForm from '~/components/common/forms/events';

export default function EditEvent() {
  return (
    <div className="p-8 space-y-8">
      <h2 className="text-4xl font-medium">Edit Event</h2>
      <EventForm />
    </div>
  );
}
