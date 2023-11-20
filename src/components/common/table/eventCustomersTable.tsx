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
import { useRouter } from 'next/router';
import { CSVLink } from 'react-csv';
import NextImage from '~/components/ui/img';
import Link from 'next/link';

export type EventCustomerType = {
  event_id: number;
  price: number;
  discount_amount: number;
  thumb: string;
  event_name: string;
  end_date: Date;
  customer_id: number;
  email: string;
  phone_number: string;
  first_name: string;
  last_name: string;
  quantity: number;
};

export default function OrdersDataTable() {
  const [sorting, setSorting] = useState<SortingState>([]);
  const [columnVisibility, setColumnVisibility] = useState<VisibilityState>({});
  const [rowSelection, setRowSelection] = useState({});

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

  const eventCustomerData = React.useMemo(() => {
    return Array.isArray(data?.data) && data?.data?.length ? data?.data : [];
  }, [data]);

  const columns: ColumnDef<EventCustomerType>[] = [
    {
      accessorKey: 'Product',
      header: 'Product',
      cell: ({ row }) => {
        return (
          <div className="flex items-center gap-4 text-ellipsis whitespace-nowrap overflow-hidden">
            <NextImage
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
        <Link href={`/admin/customers/detail/${row?.original?.customer_id}`}>
          <div className="w-40 capitalize text-ellipsis whitespace-nowrap ">
            {row?.original?.first_name}
          </div>
        </Link>
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
      accessorKey: 'Phone No.',
      header: 'Phone No.',
      cell: ({ row }) => (
        <div className="w-32 text-ellipsis whitespace-nowrap ">
          {row?.original?.phone_number ?? 'N/A'}
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
      accessorKey: 'Sub Total',
      header: 'Sub Total',
      cell: ({ row }) => (
        <p className="w-24 text-center text-ellipsis whitespace-nowrap ">
          {(row?.original?.quantity * row?.original?.price)?.toFixed(2)}
        </p>
      ),
    },

    {
      accessorKey: 'Discount',
      header: 'Discount',
      cell: ({ row }) => (
        <p className="w-24 text-center text-ellipsis whitespace-nowrap ">
          {row?.original?.discount_amount
            ? row?.original?.discount_amount?.toFixed(2)
            : 'N/A'}
        </p>
      ),
    },
    {
      accessorKey: 'Total Amount',
      header: 'Total Amount',
      cell: ({ row }) => (
        <p className="w-24 text-center text-ellipsis whitespace-nowrap ">
          {(
            row?.original?.quantity * row?.original?.price -
            row?.original?.discount_amount
          )?.toFixed(2)}
        </p>
      ),
    },
  ];
  const table = useReactTable({
    data: eventCustomerData as EventCustomerType[],
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

  const csvData = [
    [
      'Product',
      'Customer Name',
      'Customer Email',
      'Phone No.',
      'Price',
      'Quantity',
      'Sub Total',
      'Discount',
      'Total Amount',
    ],
    ...eventCustomerData?.map(
      ({
        event_name,
        first_name,
        email,
        phone_number,
        price,
        quantity,
        discount_amount,
      }) => [
        event_name,
        first_name,
        email,
        phone_number ?? 'N/A',
        price,
        quantity,
        quantity * price,
        discount_amount,
        quantity * price - discount_amount,
      ],
    ),
  ];

  return (
    <div className="w-full space-y-4">
      <div className="flex items-center justify-between">
        {eventCustomerData?.length ? (
          <Button variant="outline">
            <CSVLink filename="product-customers.csv" data={csvData}>
              Export to CSV
            </CSVLink>
          </Button>
        ) : (
          <div />
        )}

        <div className="flex items-center gap-2">
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
          {/* <TableFilters
          inputList={roleOptions1}
          item_name={'Event Customer'}
          value={filterID}
          setValue={setFilterID}
          // setFilters={setFilters}
        /> */}
        </div>
      </div>
      <div className="rounded-md border border-border">
        <ScrollArea className="w-full ">
          <ScrollBar orientation="horizontal"></ScrollBar>
          <Table>
            <TableHeader className="bg-secondary/80">
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

      <LoadingDialog open={isLoading} text={'Loading data...'} />
    </div>
  );
}
