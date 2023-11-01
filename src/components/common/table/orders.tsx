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
import { ChevronDown, MoreHorizontal } from 'lucide-react';

import { Button } from '@/ui/button';
import {
  DropdownMenu,
  DropdownMenuCheckboxItem,
  DropdownMenuContent,
  DropdownMenuItem,
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
import { GetEventSchema } from '~/schema/event';
import { ScrollArea, ScrollBar } from '~/components/ui/scroll-area';
import { LoadingDialog } from '../modal/loadingModal';
import { TableFilters } from './table_filters';
import { displayDate } from '~/utils/helper';
import { OrderViewDialog } from '../modal/orderView';
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from '~/components/ui/select';
import {
  DoubleArrowLeftIcon,
  DoubleArrowRightIcon,
} from '@radix-ui/react-icons';
import { ChevronLeftIcon, ChevronRightIcon } from '@radix-ui/react-icons';
import { CSVLink } from 'react-csv';
export type Order = {
  id: number;
  email: string;
  first_name: string;
  last_name: string;
  phone_number: string;
  total_amount: number;
  discount_amount: number;
  sub_total_amount: number;
  total_payment_id: string;
  street_address: string;
  apartment?: string;
  country: string;
  state: string;
  city: string;
  postal_code: string;
  dob: Date;
  created_at: Date;
  updated_at: Date;
};

const initialFilters: any = {
  first: 0,
  rows: 10,
  lang_id: 1,
};

export default function OrdersDataTable() {
  const [selectedItem, setSelectedItem] = useState({});
  const [title, setTitle] = useState('');
  const [type, setType] = useState('');
  const [isModal, setIsModal] = useState(false);
  const [sorting, setSorting] = useState<SortingState>([]);
  const [filters, setFilters] = useState<GetEventSchema>(initialFilters);
  const [columnVisibility, setColumnVisibility] = useState<VisibilityState>({});
  const [rowSelection, setRowSelection] = useState({});
  const [filterID, setFilterID] = useState({});

  const { data, isLoading } = trpc.order.get.useQuery(
    { ...filters, filters: { ...filterID } },
    {
      refetchOnWindowFocus: false,
    },
  );
  const orderData = React.useMemo(() => {
    return Array.isArray(data?.data) && data?.data?.length ? data?.data : [];
  }, [data]);
  const handleView = (data: any, type: string) => {
    setSelectedItem(data);
    setTitle('Banner');
    setType(type);
    setIsModal(true);
  };

  const columns: ColumnDef<Order>[] = [
    {
      id: 'ID',
      header: 'Order ID',
      enableHiding: false,
      cell: ({ row }) => {
        return (
          <DropdownMenu>
            <DropdownMenuTrigger asChild>
              <Button variant="ghost" className="max-w-fit px-2">
                <span className="sr-only">Open menu</span>
                <div className=" hover:text-primary w-24 text-left ">
                  # {row.original.id}
                </div>
                {/* <MoreHorizontal className="h-4 w-4" /> */}
              </Button>
            </DropdownMenuTrigger>
            <DropdownMenuContent align="end">
              <DropdownMenuItem
                onClick={() => handleView(row?.original, 'view')}
              >
                View Order
              </DropdownMenuItem>
            </DropdownMenuContent>
          </DropdownMenu>
        );
      },
    },

    {
      accessorKey: 'First Name',
      header: 'First Name',
      cell: ({ row }) => (
        <div className="capitalize text-ellipsis whitespace-nowrap  overflow-hidden w-24">
          {row?.original?.first_name}
        </div>
      ),
    },
    {
      accessorKey: 'Last Name',
      header: 'Last Name',
      cell: ({ row }) => (
        <div className="capitalize text-ellipsis whitespace-nowrap  overflow-hidden w-24">
          {row?.original?.last_name}
        </div>
      ),
    },
    {
      accessorKey: 'Created at',
      header: 'Created at',
      cell: ({ row }) => (
        <div className="capitalize text-ellipsis whitespace-nowrap overflow-hidden ">
          {displayDate(row?.original?.created_at)}
        </div>
      ),
    },

    {
      accessorKey: 'Email',
      header: 'Email',
      cell: ({ row }) => (
        <div className=" text-ellipsis whitespace-nowrap ">
          {row?.original?.email}
        </div>
      ),
    },

    {
      accessorKey: 'Phone No',
      header: 'Phone No.',
      cell: ({ row }) => (
        <div className="capitalize text-ellipsis whitespace-nowrap ">
          {row?.original?.phone_number}
        </div>
      ),
    },
    // {
    //   accessorKey: 'Transaction ID',
    //   header: 'Transaction ID',
    //   cell: ({ row }) => (
    //     <div className=" text-ellipsis whitespace-nowrap ">
    //       {row?.original?.total_payment_id}
    //     </div>
    //   ),
    // },
    // {
    //   accessorKey: 'DOB',
    //   header: 'DOB',
    //   cell: ({ row }) => (
    //     <div className="capitalize text-ellipsis whitespace-nowrap ">
    //       {displayDate(row?.original?.dob)}
    //     </div>
    //   ),
    // },
    // {
    //   accessorKey: 'Country',
    //   header: 'Country',
    //   cell: ({ row }) => (
    //     <div className=" text-ellipsis whitespace-nowrap ">
    //       {row?.original?.country}
    //     </div>
    //   ),
    // },
    // {
    //   accessorKey: 'State',
    //   header: 'State',
    //   cell: ({ row }) => (
    //     <div className=" text-ellipsis whitespace-nowrap ">
    //       {row?.original?.state}
    //     </div>
    //   ),
    // },
    // {
    //   accessorKey: 'City',
    //   header: 'City',
    //   cell: ({ row }) => (
    //     <div className=" text-ellipsis whitespace-nowrap ">
    //       {row?.original?.city}
    //     </div>
    //   ),
    // },
    // {
    //   accessorKey: 'Street Address',
    //   header: 'Street Address',
    //   cell: ({ row }) => (
    //     <div className=" text-ellipsis whitespace-nowrap ">
    //       {row?.original?.street_address}
    //     </div>
    //   ),
    // },
    // {
    //   accessorKey: 'Apartment',
    //   header: 'Apartment',
    //   cell: ({ row }) => (
    //     <div className=" text-ellipsis whitespace-nowrap ">
    //       {row?.original?.apartment ? row?.original?.apartment : 'N/A'}
    //     </div>
    //   ),
    // },
    {
      accessorKey: 'Sub Total',
      header: 'Sub Total',
      cell: ({ row }) => (
        <div className="capitalize text-ellipsis whitespace-nowrap ">
          AED {(row?.original?.sub_total_amount).toFixed(2)}
        </div>
      ),
    },
    {
      accessorKey: 'Discount',
      header: 'Discount',
      cell: ({ row }) => (
        <div className="capitalize text-ellipsis whitespace-nowrap ">
          {' '}
          {row?.original?.discount_amount > 0
            ? 'AED ' + (row?.original?.discount_amount).toFixed(2)
            : 'N/A'}
        </div>
      ),
    },
    {
      accessorKey: 'Total Amount',
      header: 'Total Amount',
      cell: ({ row }) => (
        <div className="capitalize text-ellipsis whitespace-nowrap w-28">
          {' '}
          {row?.original?.total_amount > 0
            ? 'AED ' + (row?.original?.total_amount).toFixed(2)
            : 'N/A'}
        </div>
      ),
    },
  ];

  const table = useReactTable({
    data: orderData as Order[],
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

  function handlePagination(page: number) {
    if (page < 0) return;
    setFilters((prevFilters) => ({ ...prevFilters, first: page }));
  }
  const roleOptions1 = [
    {
      Icon: 'fal fa-chevron-down',
      text: 'Search',
      filtername: 'searchQuery',
      type: 'text',
    },

    {
      Icon: 'fal fa-chevron-down',
      text: 'From Date',
      filtername: 'startDate',
      type: 'date',
    },
    {
      Icon: 'fal fa-chevron-down',
      text: 'To Date',
      filtername: 'endDate',
      type: 'date',
    },
    {
      Icon: 'fal fa-chevron-down',
      text: 'Clear Filter',
      filtername: 'Clear',
    },
  ];

  const csvData = [
    [
      'First Name',
      'Last Name',
      'Email',
      'Phone No.',
      'DOB',
      'Transaction ID',
      'Country',
      'State',
      'City',
      'Street Address',
      'Apartment',
      'Sub Total',
      'Discount',
      'Total Amount',
      'Created at',
    ],
    ...orderData?.map(
      ({
        first_name,
        last_name,
        email,
        phone_number,
        dob,
        total_payment_id,
        country,
        state,
        city,
        street_address,
        apartment,
        sub_total_amount,
        discount_amount,
        total_amount,
        created_at,
      }) => [
        first_name,
        last_name,
        email,
        phone_number,
        dob?.toLocaleDateString(),
        total_payment_id,
        country,
        state,
        city,
        street_address,
        apartment ? apartment : 'N/A',

        sub_total_amount,
        discount_amount ? discount_amount : 'N/A',
        total_amount,
        created_at?.toLocaleDateString(),
      ],
    ),
  ];

  return (
    <div className="w-full space-y-4">
      <div className="flex items-center justify-between">
        {orderData?.length ? (
          <Button variant="outline">
            <CSVLink filename="orders.csv" data={csvData}>
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
          <TableFilters
            inputList={roleOptions1}
            item_name={'Orders'}
            value={filterID}
            setValue={setFilterID}
            setFilters={setFilters}
            initial={initialFilters}
          />
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
      <div className="flex items-center justify-end space-x-2 py-4">
        <div className="flex-1 flex w-[100px] items-center justify-start text-sm font-medium">
          Page {filters.first + 1} of{' '}
          {Math.ceil((data?.count ?? 0) / filters.rows)}
        </div>

        <div className="flex items-center justify-center space-x-6 lg:space-x-8">
          <div className="flex items-center space-x-2">
            <p className="text-sm font-medium">Rows per page</p>
            <Select
              value={`${filters.rows}`}
              onValueChange={(value) => {
                setFilters((prevFilters: any) => ({
                  ...prevFilters,
                  rows: Number(value),
                  first: 0,
                }));
                table.setPageSize(Number(value));
              }}
            >
              <SelectTrigger className="h-8 w-[70px]">
                <SelectValue placeholder={filters.rows} />
              </SelectTrigger>
              <SelectContent side="top">
                {[5, 10, 20, 30, 40, 50].map((pageSize) => (
                  <SelectItem key={pageSize} value={`${pageSize}`}>
                    {pageSize}
                  </SelectItem>
                ))}
              </SelectContent>
            </Select>
          </div>

          <div className="flex items-center space-x-2">
            <Button
              variant="outline"
              className="hidden h-8 w-8 p-0 lg:flex"
              onClick={() => handlePagination(0)}
              disabled={filters.first === 0}
            >
              <span className="sr-only">Go to first page</span>
              <DoubleArrowLeftIcon className="h-4 w-4" />
            </Button>

            <Button
              variant="outline"
              className="h-8 w-8 p-0"
              onClick={() => handlePagination(filters?.first - 1)}
              disabled={filters?.first === 0}
            >
              <span className="sr-only">Go to previous page</span>
              <ChevronLeftIcon className="h-4 w-4" />
            </Button>
            <Button
              variant="outline"
              className="h-8 w-8 p-0"
              onClick={() => handlePagination(filters.first + 1)}
              disabled={
                (filters.first + 1) * filters.rows > (data?.count ?? 0) ||
                Math.ceil((data?.count ?? 0) / filters.rows) ==
                  filters.first + 1
              }
            >
              <span className="sr-only">Go to next page</span>
              <ChevronRightIcon className="h-4 w-4" />
            </Button>

            <Button
              variant="outline"
              className="hidden h-8 w-8 p-0 lg:flex"
              onClick={() =>
                handlePagination(
                  Math.ceil((data?.count ?? 0) / filters.rows) - 1,
                )
              }
              disabled={
                (filters.first + 1) * filters.rows > (data?.count ?? 0) ||
                Math.ceil((data?.count ?? 0) / filters.rows) ==
                  filters.first + 1
              }
            >
              <span className="sr-only">Go to last page</span>
              <DoubleArrowRightIcon className="h-4 w-4" />
            </Button>
          </div>
        </div>
      </div>
      <OrderViewDialog
        selectedItem={selectedItem}
        setSelectedItem={setSelectedItem}
        title={title}
        setTitle={setTitle}
        isModal={isModal}
        setIsModal={setIsModal}
        type={type}
        setType={setType}
      />
      <LoadingDialog open={isLoading} text={'Loading data...'} />
    </div>
  );
}
