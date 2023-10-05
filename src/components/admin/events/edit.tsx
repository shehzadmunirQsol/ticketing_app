import EventForm from '~/components/common/forms/events';
import GlobalBack from '~/components/common/globalBack';

export default function EditEvent() {
  return (
    <div className="p-8 space-y-8">
      <div className="flex items-center justify-between">
        <h2 className="text-4xl font-medium">Edit Product</h2>
        <GlobalBack />
      </div>
      <EventForm />
    </div>
  );
}
