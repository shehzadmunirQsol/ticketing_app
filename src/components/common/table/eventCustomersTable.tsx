import React, { useState } from 'react';
import {
  ColumnDef,
  // ColumnFiltersState,
  SortingState,
  VisibilityState,
  flexRender,
  getCoreRowModel,
  getFilteredRowModel,
  getPaginationRowModel,
  getSortedRowModel,
  useReactTable,
} from '@tanstack/react-table';
import { ChevronDown } from 'lucide-react';

import { Button } from '@/ui/button';
import {
  DropdownMenu,
  DropdownMenuCheckboxItem,
  DropdownMenuContent,
  DropdownMenuItem,
  DropdownMenuLabel,
  DropdownMenuSeparator,
  DropdownMenuTrigger,
} from '@/ui/dropdown-menu';
import {
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableHeader,
  TableRow,
} from '@/ui/table';
import { trpc } from '~/utils/trpc';
import { ScrollArea, ScrollBar } from '~/components/ui/scroll-area';
import { LoadingDialog } from '../modal/loadingModal';
import Image from 'next/image';
import { useRouter } from 'next/router';
import { MoreHorizontal } from 'lucide-react';
import { SelectWinnerDialog } from '../modal/eventModal';

export type EventCustomerType = {
  event_id: number;
  price: number;
  thumb: string;
  event_name: string;
  end_date: Date;
  customer_id: number;
  email: string;
  first_name: string;
  last_name: string;
  quantity: number;
};

const initialModalProps = {
  isModal: false,
  event_id: 0,
  customer_id: 0,
  event_name: '',
  customer_name: '',
  customer_email: '',
};

export default function OrdersDataTable() {
  const [sorting, setSorting] = useState<SortingState>([]);
  const [columnVisibility, setColumnVisibility] = useState<VisibilityState>({});
  const [rowSelection, setRowSelection] = useState({});

  const [modalProps, setModalProps] = useState(initialModalProps);

  const router = useRouter();
  const event_id =
    router?.query?.event_id && +router?.query?.event_id > 0
      ? +router?.query?.event_id
      : 0;

  const { data, isLoading } = trpc.event.getEventCustomers.useQuery(
    { event_id },
    {
      refetchOnWindowFocus: true,
      enabled: event_id > 0 ? true : false,
    },
  );

  const cartItemData = React.useMemo(() => {
    return Array.isArray(data?.data) ? data?.data : [];
  }, [data]);

  const columns: ColumnDef<EventCustomerType>[] = [
    {
      accessorKey: 'Event',
      header: 'Event',
      cell: ({ row }) => {
        return (
          <div className="flex items-center gap-4 text-ellipsis whitespace-nowrap overflow-hidden">
            <Image
              className="object-cover bg-ac-2 h-10 w-16 rounded-lg"
              src={`${process.env.NEXT_PUBLIC_MEDIA_BASE_URL}${row?.original?.thumb}`}
              alt={row?.original?.event_name ?? ''}
              width={100}
              height={100}
            />

            <p className="w-40 text-ellipsis whitespace-nowrap overflow-hidden">
              {row?.original?.event_name}
            </p>
          </div>
        );
      },
    },
    {
      accessorKey: 'Customer Name',
      header: 'Customer Name',
      cell: ({ row }) => (
        <div className="capitalize text-ellipsis whitespace-nowrap ">
          {row?.original?.first_name}
        </div>
      ),
    },
    {
      accessorKey: 'Customer Email',
      header: 'Customer Email',
      cell: ({ row }) => (
        <div className="text-ellipsis whitespace-nowrap ">
          {row?.original?.email}
        </div>
      ),
    },

    {
      accessorKey: 'Price',
      header: 'Price',
      cell: ({ row }) => (
        <p className="w-16 text-ellipsis whitespace-nowrap ">
          {row?.original?.price?.toFixed(2)}
        </p>
      ),
    },
    {
      accessorKey: 'Quantity',
      header: 'Quantity',
      cell: ({ row }) => (
        <p className="w-16 text-center text-ellipsis whitespace-nowrap ">
          {row?.original?.quantity}
        </p>
      ),
    },
    {
      accessorKey: 'Total Amount',
      header: 'Total Amount',
      cell: ({ row }) => (
        <p className="w-24 text-center text-ellipsis whitespace-nowrap ">
          {(row?.original?.quantity * row?.original?.price)?.toFixed(2)}
        </p>
      ),
    },
    {
      id: 'actions',
      enableHiding: false,
      cell: ({ row }) => {
        return new Date() > row?.original?.end_date ? (
          <DropdownMenu>
            <DropdownMenuTrigger asChild>
              <Button variant="ghost" className="h-8 w-8 p-0">
                <span className="sr-only">Open menu</span>
                <MoreHorizontal className="h-4 w-4" />
              </Button>
            </DropdownMenuTrigger>
            <DropdownMenuContent align="end">
              <DropdownMenuLabel>Actions</DropdownMenuLabel>
              <DropdownMenuSeparator />
              <DropdownMenuItem
                onClick={() => setModalPropsHandler(row.original)}
              >
                Select Winner
              </DropdownMenuItem>
            </DropdownMenuContent>
          </DropdownMenu>
        ) : null;
      },
    },
  ];
  const table = useReactTable({
    data: cartItemData as EventCustomerType[],
    columns,
    onSortingChange: setSorting,
    getCoreRowModel: getCoreRowModel(),
    getPaginationRowModel: getPaginationRowModel(),
    getSortedRowModel: getSortedRowModel(),
    getFilteredRowModel: getFilteredRowModel(),
    onColumnVisibilityChange: setColumnVisibility,
    onRowSelectionChange: setRowSelection,
    state: {
      sorting,
      columnVisibility,
      rowSelection,
    },
  });

  function setModalPropsHandler(params: EventCustomerType) {
    setModalProps({
      customer_id: params.customer_id,
      event_id: params.event_id,
      event_name: params.event_name,
      customer_name: params.first_name,
      customer_email: params.email,
      isModal: true,
    });
  }
  function openChangeHandler() {
    setModalProps((prevState) => ({
      ...initialModalProps,
      isModal: !prevState.isModal,
    }));
  }

  return (
    <div className="w-full space-y-4">
      <div className="flex items-center justify-end gap-2">
        <DropdownMenu>
          <DropdownMenuTrigger asChild>
            <Button variant="outline">
              Columns <ChevronDown className="ml-2 h-4 w-4" />
            </Button>
          </DropdownMenuTrigger>
          <DropdownMenuContent align="end">
            {table
              .getAllColumns()
              .filter((column) => column.getCanHide())
              .map((column) => {
                return (
                  <DropdownMenuCheckboxItem
                    key={column.id}
                    className="capitalize"
                    checked={column.getIsVisible()}
                    onCheckedChange={(value) =>
                      column.toggleVisibility(!!value)
                    }
                  >
                    {column.id}
                  </DropdownMenuCheckboxItem>
                );
              })}
          </DropdownMenuContent>
        </DropdownMenu>
      </div>
      <div className="rounded-md border border-border">
        <ScrollArea className="w-full ">
          <ScrollBar orientation="horizontal"></ScrollBar>
          <Table>
            <TableHeader>
              {table?.getHeaderGroups().map((headerGroup) => (
                <TableRow key={headerGroup.id}>
                  {headerGroup.headers.map((header) => {
                    return (
                      <TableHead key={header.id}>
                        {header.isPlaceholder
                          ? null
                          : flexRender(
                              header.column.columnDef.header,
                              header.getContext(),
                            )}
                      </TableHead>
                    );
                  })}
                </TableRow>
              ))}
            </TableHeader>
            <TableBody>
              {table?.getRowModel()?.rows?.length ? (
                table?.getRowModel()?.rows?.map((row) => (
                  <TableRow
                    key={row.id}
                    data-state={row.getIsSelected() && 'selected'}
                  >
                    {row.getVisibleCells().map((cell) => (
                      <TableCell key={cell.id}>
                        {flexRender(
                          cell.column.columnDef.cell,
                          cell.getContext(),
                        )}
                      </TableCell>
                    ))}
                  </TableRow>
                ))
              ) : (
                <TableRow>
                  <TableCell
                    colSpan={columns.length}
                    className="h-24 text-center"
                  >
                    No results.
                  </TableCell>
                </TableRow>
              )}
            </TableBody>
          </Table>
        </ScrollArea>
      </div>

      <SelectWinnerDialog
        {...modalProps}
        openChangeHandler={openChangeHandler}
      />
      <LoadingDialog open={isLoading} text={'Loading data...'} />
    </div>
  );
}
